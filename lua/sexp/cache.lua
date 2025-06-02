
local ApiPos = require'sexp.pos'

-- DEBUG/PROFILING
local prof = require'sexp.prof'
local reltime = vim.fn.reltime
local reltimestr = vim.fn.reltimestr
local dbg = require'dp':get('sexp', {enabled=true})

---@alias PosKey string
---@alias TSNodeType string
---@alias RgnKey string

---@class BufCache
---@field changedtick integer?
---@field hit {beg: ApiPos, end_: ApiPos}
local BufCache = {}
BufCache.__index = BufCache

function BufCache:new()
	local o = {
    changedtick = vim.api.nvim_buf_get_changedtick(0),
	}
	return setmetatable(o, BufCache)
end

---@return boolean valid # true iff BufCache valid
function BufCache:is_valid()
  return self.changedtick == vim.api.nvim_buf_get_changedtick(0)
end

---@param node TSNode
---@param key string
function BufCache:add_hit(node, key)
  local beg, end_ = ApiPos:range_of_tsnode(node)
  -- This may overwrite existing.
  -- TODO: Consider making a Range type.
  self.hit = {
    beg = beg,
    end_ = end_,
    key = key,
  }
end

---@param pos ApiPos
---@param keys string[]? # keys of interest (nil for all keys)
---@return string|false|nil
function BufCache:lookup(pos, keys)
  --ts = reltime()
  local ret = self.hit and self.hit.beg <= pos and self.hit.end_ > pos or nil
  --prof:add("Hit check", reltimestr(reltime(ts)))
  return ret
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
---@param keys string[]?
---@return string|false|nil
function Cache:lookup(pos, keys)
  --local ts = reltime()
  local bcache = self:get_buf_cache()
  --prof:add("get_buf_cache", reltimestr(reltime(ts)))
  --dbg:logf("Get bcache took %s", reltimestr(reltime(ts)))
  --ts = reltime()
  local ret = bcache and bcache:lookup(pos, keys)
  --prof:add("bcache:lookup", reltimestr(reltime(ts)))
  return ret
end

function Cache:show()
  dbg:logf("cache: %s", vim.inspect(self))
end

return Cache

-- vim:ts=2:sw=2:et:tw=90:ai:si
