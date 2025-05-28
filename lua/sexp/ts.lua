local M = {}

local dbg = require'dp':get('sexp', {enabled=true})

---@class ApiPos
---@field row integer
---@field col integer
local ApiPos = {}
ApiPos.__index = ApiPos

---@param row integer?
---@param col integer?
function ApiPos:new(row, col)
  local o = {row = row or 0, col = col or 0}
  return setmetatable(o, ApiPos)
end

---@param node TSNode
---@return ApiPos?
---@return ApiPos?
function ApiPos:range_of_tsnode(node)
  if not node then
    return nil
  end
  local sr, sc, er, ec = node:range()
  return ApiPos:new(sr, sc), ApiPos:new(er, ec)
end

function ApiPos:__lt(rhs)
  return self.row < rhs.row or self.row == rhs.row and self.col < rhs.col
end

function ApiPos:__eq(rhs)
  return self.row == rhs.row and self.col == rhs.col
end

---@return integer[]
function ApiPos:positions()
  return {self.row, self.col}
end

function ApiPos:__tostring()
  return "[" .. self.row .. ", " .. self.col .. "]"
end

---@alias BufCache {ts_id: string?, changedtick: integer, beg: ApiPos, end_: ApiPos, captures: string[]?}
---@alias Cache table<integer, BufCache>

---@type Cache
local cache = {}

-- Return true iff at least one of the captures at line/col matches the input pattern.
---@param patt string # TODO: Consider constructing this ourselves from bitmask
---@param line integer # 1-based line number
---@param col integer # 1-based col number
---@return boolean? matched # true/false if definitely in/not in rgn, nil for unknown
function M.is_rgn_type(patt, line, col)
  --dbg:logf("%d, %d: %s", line, col, patt)
  -- Note: get_captures_at_pos requires Neovim 0.9. I believe it existed under other names
  -- prior to that, but version-specific logic isn't warranted, given that Neovim
  -- lispers/schemers will almost certainly have something at least that recent.
  if not vim.fn.exists('*vim.treesitter.get_node') then
    return nil
  end
  -- Grab api-indexed pos that supports comparison operators.
  local pos = ApiPos:new(line-1, col-1)

  dbg:logf("is_rgn_type(%s, %d, %d): %d %d", patt, line, col, unpack(pos:positions()))
  -- TODO: Probably get_parser (and parse()?) first? Could at least cache whethere those
  -- calls are necessary if they're expensive...
  ---@type TSNode?
  local node = vim.treesitter.get_node({pos = pos:positions()})

  local ret = node and vim.fn.match(node:type(), patt) >= 0
  --dbg:logf("is_rgn_type(%s, %d, %d) => %s", patt, line, col, ret)
  return ret
end

--[[
local function invalidate_cache()
  cache = {}
end

---@param pos ApiPos
---@return string?
local function get_cached_node(pos)
  -- Can we guarantee the cache is still valid?
  if vim.api.nvim_buf_get_changedtick(0) == cache.changedtick then
    -- Is the position of interest within the cached range?
    if pos >= cache.beg and pos < cache.end_ then
      return cache.ts_id
    end
  end
end

---@param pos ApiPos?
---@return boolean
local function is_cache_valid(pos)
  return cache.ts_id
    and vim.api.nvim_buf_get_changedtick(0) == cache.changedtick
    and (not pos or pos >= cache.beg and pos < cache.end_)
    or false
end

---@param pos ApiPos?
---@return string? ts_id
---@return string[]? captures
local function get_cache(pos)
  -- Can we guarantee the cache is still valid?
  if is_cache_valid(pos) then
      return cache.ts_id, cache.captures
  end
  return nil, nil
end

-- Cache the provided list of captures (possibly nil or empty), along with information
-- that may be used subsequently to determine the cache's applicability to a particular
-- query. Even if captures is not provide, we attempt to get (and cache) node at pos,
-- since client may need to know whether empty capture list is due to lack of node.
---@param pos ApiPos
---@param captures string[]?
---@return boolean success # True iff cache was updated
local function update_cache(pos, captures)
  -- Get the lowest node under the cursor.
  -- Rationale: Every position within this node will be associated with the same captures.
  -- Note: If buffer is not parsed, get_node will return nil (or nothing). In either case,
  -- there's no point in caching because we'll have no range to associate with the cache.
  ---@type TSNode?
  local node = vim.treesitter.get_node({pos = pos:positions()})
  if not node then
    -- Invalidate cache.
    invalidate_cache()
    return false
  end
  -- Design Decision: Don't overwrite valid captures with nil.
  if not is_cache_valid() or captures then
    -- Update cache.
    cache.ts_id = node:id()
    cache.beg = ApiPos:new(node:start())
    cache.end_ = ApiPos:new(node:end_())
    cache.captures = captures
    cache.changedtick = vim.api.nvim_buf_get_changedtick(0)
    dbg:logf("Updated cached for beg=%s end=%s", cache.beg, cache.end_)
  end
  return true
end
---@param node TSNode
---@return ApiPos, ApiPos
---@diagnostic disable-next-line
local function get_safe_range(node)
  -- This range may shrink, but not expand.
  ---@type ApiPos, ApiPos
  local beg, end_ = ApiPos:range_of_tsnode(node)
  local prev, next = node:prev_sibling(), node:next_sibling()
  if prev then
    b = ApiPos:new(prev:end_())
    -- Look for 
  end
  if next then
    e = ApiPos:new(next:start())
  end
end
]]

--[[
---@param pos ApiPos
---@return string[]?
---@diagnostic disable-next-line
local function get_captures(pos)
  local bufnr = vim.api.nvim_get_current_buf()
  ---@type BufCache
  local bcache = cache[bufnr]
  local captures
  local cached
  if bcache then
    -- Something's cached for this buffer. Is it valid for input position?
    if vim.api.nvim_buf_get_changedtick(0) == bcache.changedtick then
      -- Is the position of interest within the cached range?
      if pos >= bcache.beg and pos < bcache.end_ then
        cached = true
        captures = bcache.captures
      end
    end
  end
  if not captures then
    -- Request can't be satisfied from cache. Get captures at position of interest.
    captures = vim.iter(
      vim.treesitter.get_captures_at_pos(0, pos.row, pos.col)
    ):map(function (c) return c.capture end):totable()
    ---@type TSNode?
    local node = vim.treesitter.get_node({pos = pos:positions()})
    if node then
      -- Determine a range within which we know captures will be unchanged.
      bcache.beg, bcache.end_ = get_safe_range(node)
      if not bcache.beg then
        -- Can't get safe range so don't cache.
        bcache.ts_id = nil
      end
    else
      -- Invalidate node portion of buf cache.
      -- TODO: Skip treesitter get_node until changedtick change.
      bcache.ts_id = nil
      bcache.changedtick = vim.api.nvim_buf_get_changedtick(0)
    end
  end
  if not cached then
    -- Get the lowest node under the cursor.
    -- Note: If buffer is not parsed, get_node will return nil (or nothing). In either case,
    -- there's no point in caching because we'll have no range to associate with the cache.
  end
  return captures
end
]]

return M

-- vim:ts=2:sw=2:et:tw=90:ai:si
