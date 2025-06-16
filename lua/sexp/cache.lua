
local ApiPos = require'sexp.pos'
local ApiRange = require'sexp.range'

-- DEBUG/PROFILING
local prof = require'sexp.prof'
local reltime = vim.fn.reltime
local reltimefloat = vim.fn.reltimefloat
local reltimestr = vim.fn.reltimestr
local dbg = require'dp':get('sexp', {enabled=false})

---@class CacheHit
---@field node TSNode
---@field key string
---@field range ApiRange

---@alias PosKey string
---@alias TSNodeType string
---@alias RgnKey string

---@class BufCache
---@field changedtick integer?
---@field root TSNode?
---@field hit CacheHit
local BufCache = {}
BufCache.__index = BufCache

---@return TSNode? root # root of the current buffer's primary ts tree, else nil
local function get_root()
  -- Note: Shouldn't really need pcall here, but the 'error' option is slated for removal
  -- in v0.12, and though it *shouldn't* cause issues at this point, I don't like to
  -- assume.
  local ok, parser = pcall(vim.treesitter.get_parser, nil, nil, {error = false})
  --dbg:logf("ok=%s parser=%s", vim.inspect(ok), vim.inspect(parser))
  if not ok or not parser then
    return nil
  end
  local trees = parser:parse()
  if not trees then
    return nil
  end
  --dbg:logf("get_root called for buffer %d: %s", vim.api.nvim_get_current_buf(), trees[1]:root():sexpr())
  -- Return the root of the primary tree.
  return trees[1]:root()
end

function BufCache:new()
	local o = {
    changedtick = vim.api.nvim_buf_get_changedtick(0),
    root = get_root(),
	}
	return setmetatable(o, BufCache)
end

---@return TSNode? root # root TSNode of buffer, else nil
function BufCache:get_root()
  return self.root
end

-- Design Decision: For now, consider only changedtick, not existence of root.
-- Rationale: Ok to cache fact that a buffer has no tree, at least till next change;
-- methods that need a tree can check root.
---@return boolean? valid # true/false indicating buf validity, else nil if no root
function BufCache:is_valid()
  return self.changedtick == vim.api.nvim_buf_get_changedtick(0)
end

---@param node TSNode
---@param key string # the key to cache with the node
function BufCache:add_hit(node, key)
  -- This may overwrite existing.
  self.hit = {
    node = node,
    range = ApiRange:from_node(node),
    key = key,
  }
end

---@param pos ApiPos
---@return string|false|nil
---@return TSNode?
function BufCache:lookup(pos)
  --ts = reltime()
  -- self.root check would be superfluous.
  if self.hit and self.hit.range:contains(pos) then
    -- Match!
    return self.hit.key, self.hit.node
  end
  -- No match, but return false and node at position of interest if we can.
  if self.root then
    local row, col = pos:positions()
    local ts = reltime()
    local node = self.root:named_descendant_for_range(row, col, row, col + 1)
    local tsf = reltimefloat(reltime(ts))
    dbg:logf("BufCache:lookup took %f", tsf)
    if node then
      return false, node
    end
  end
  -- No match and no node to return.
  return nil
  --prof:add("Hit check", reltimestr(reltime(ts)))
end

-- TODO: Consider putting this in separate file.
---@class Cache
---@field bcaches table<integer, BufCache>
local Cache = {}
Cache.__index = Cache

function Cache:new()
  local o = {
    bcaches = {}
  }
  -- Note: No need for an augroup: there should never be more than one Cache instance, and
  -- there's no reason to unregister it before Vim shutdown.
  vim.api.nvim_create_autocmd({"BufUnload"}, {
    callback = function(ev)
      -- Make sure unloaded buffer isn't cached.
      o.bcaches[ev.buf] = nil
    end
  })
  return setmetatable(o, Cache)
end

-- Return BufCache for current buffer or nil.
-- Note: If create flag is not set, BufCache will be created if necessary.
---@param create boolean?
---@return BufCache?
function Cache:get_buf_cache(create)
  local buf_id = vim.api.nvim_get_current_buf()
  ---@type BufCache?
  local bcache = self.bcaches[buf_id]
  if not bcache or not bcache:is_valid() then
    -- If caller needs the cache to exist (create == true), create it, and return nil,
    -- freeing any (invalid) buf cache that exists.
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
  -- Assumption: This method is called only when buf cache exists.
  self:get_or_create_buf_cache():add_hit(node, key)
end

---TODO: Consider removing the 'create' arg, whose purpose is to allow check for root.
---Keeping it makes sense only if we might want to use this method only to check whether
---we've already cached something for this buffer, without actually attempting to parse if
---not. Not sure there would be a reason for that.
---@param create boolean? # true to create cache if it doesn't already exist
---@return TSNode? root # the root node of current buffer, else nil
function Cache:get_root(create)
  local bcache = self:get_buf_cache(create)
  return bcache and bcache:get_root()
end

---@param pos ApiPos
---@param create boolean?
---@return string|false|nil key # the cached key|false if miss|nil if no tree
---@return TSNode? # *any* node at pos (hit or miss), else nil if no tree
function Cache:lookup(pos, create)
  --local ts = reltime()
  local bcache = self:get_buf_cache(create)
  if bcache then
    return bcache:lookup(pos)
  end
  return nil
end

function Cache:show()
  dbg:logf("cache: %s", vim.inspect(self))
end

return Cache

-- vim:ts=2:sw=2:et:tw=90:ai:si
