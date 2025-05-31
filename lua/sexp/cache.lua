
local ApiPos = require'sexp.pos'
local prof = require'sexp.prof'

local dbg = require'dp':get('sexp', {enabled=false})

---@alias TSNodeId string
---@alias PosKey string
---@alias TSNodeType string
---@alias RgnKey string

---@class BufCache
---@field changedtick integer?
---@field misses table <PosKey, true>
---@field hits table <RgnKey, {beg: ApiPos, end_: ApiPos}>
local BufCache = {}
BufCache.__index = BufCache

function BufCache:new()
	local o = {
    changedtick = vim.api.nvim_buf_get_changedtick(0),
    misses = {},
    hits = {},
	}
	return setmetatable(o, BufCache)
end

---@return boolean valid # true iff BufCache valid
function BufCache:is_valid()
  return self.changedtick == vim.api.nvim_buf_get_changedtick(0)
end

---@param node TSNode
---@param key string
function BufCache:add_hit_faster(node, key)
  local beg, end_ = ApiPos:range_of_tsnode(node)
  -- This may overwrite existing.
  -- TODO: Consider making a Range type.
  self.hit = {
    beg = beg,
    end_ = end_,
    key = key,
  }
end

---@param node TSNode
---@param key string
function BufCache:add_hit(node, key)
  local beg, end_ = ApiPos:range_of_tsnode(node)
  -- This may overwrite existing.
  -- TODO: Consider making a Range type.
  self.hits[key] = {
    beg = beg,
    end_ = end_
  }
end

---@param pos ApiPos
function BufCache:add_miss(pos)
  self.misses[pos:__tostring()] = true
end

---@param pos ApiPos
---@param keys string[]? # keys of interest (nil for all keys)
---@return string|false|nil
function BufCache:lookup_faster(pos, keys)
  --dbg:logf("BufCache:lookup pos=%s keys=%s", pos, vim.inspect(keys))
  -- Return will be nil if pos not in any range.
  --local ts = vim.fn.reltime()
  local x = self.misses[pos:__tostring()]
  --dbg:logf("Miss check: %s", vim.fn.reltimestr(vim.fn.reltime(ts)))
  if x then
    return false
  end
  if self.hit and self.hit.beg <= pos and self.hit.end_ > pos then
    return self.hit.key
  end
  return nil
end
function BufCache:lookup(pos, keys)
  --dbg:logf("BufCache:lookup pos=%s keys=%s", pos, vim.inspect(keys))
  -- Return will be nil if pos not in any range.
  return self.misses[pos:__tostring()] and false or
    -- Discard the range, which is needed only for filtering, returning found key or nil.
    -- Note: Iterate either the hits table or just the subset corresponding to keys.
    -- Note: next() on empty iterator returns nil.
    vim.iter(not keys and
      self.hits or vim.iter(keys):map(function(k) return k, self.hits[k] end))
      :filter(function (_, v)
        --if not v then dbg:logf("Oops! No v for k = %s", key) end
        --dbg:logf("Comparing pos=%s with range %s - %s", pos, v.beg, v.end_)
        return v and v.beg <= pos and v.end_ > pos
      end):next()
end

-- TODO: Consider putting this in separate file.
---@class Cache
---@field bcaches table<integer, BufCache>
local Cache = {}
Cache.__index = Cache

-- TODO: BufUnload autocommand to free a BufCache
function Cache:new()
  local o = {
    bcaches = {}
  }
  return setmetatable(o, Cache)
end

-- Return BufCache for current buffer or nil if create flag not set and buffer has no
-- cache.
---@param create boolean?
---@return BufCache?
function Cache:get_buf_cache(create)
  local buf_id = vim.api.nvim_get_current_buf()
  ---@type BufCache?
  local bcache = self.bcaches[buf_id]
  if not bcache or not bcache:is_valid() then
    -- If caller needs the cache to exist (create == true), create it, else return nil.
    bcache = create and BufCache:new() or nil
    self.bcaches[buf_id] = bcache
  end
  return bcache
end

---@return BufCache
function Cache:get_or_create_buf_cache()
  return self:get_buf_cache(true) --[[@as BufCache]]
end

---@param node TSNode
---@param key string
function Cache:add_hit(node, key)
  self:get_or_create_buf_cache():add_hit(node, key)
end

---@param pos ApiPos
function Cache:add_miss(pos)
  self:get_or_create_buf_cache():add_miss(pos)
end

---@param pos ApiPos
---@param keys string[]?
---@return string|false|nil
function Cache:lookup(pos, keys)
  --local ts = vim.fn.reltime()
  local bcache = self:get_buf_cache()
  --dbg:logf("Get bcache took %s", vim.fn.reltimestr(vim.fn.reltime(ts)))
  return bcache and bcache:lookup(pos, keys)
end

return Cache

-- vim:ts=2:sw=2:et:tw=90:ai:si
