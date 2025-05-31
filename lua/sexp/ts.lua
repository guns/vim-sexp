local M = {}
local ApiPos = require'sexp.pos'

local dbg = require'dp':get('sexp', {enabled=true})
local prof = require'sexp.prof'

---@type Cache
local cache = require'sexp.cache':new()

-- Compile the primitive regexes.
local regexes = vim.iter({
  character = "character|char_lit",
  string = "string|str_lit",
  comment = "comment",
  regex = "regex|pattern",
}):map(function (key, patt) return key, vim.regex("\\v" .. patt) end):fold(
  {}, function (acc, k, v) acc[k] = v; return acc end)

-- Map of region to primitives
local regions = {
  string = {"string"},
  comment = {"comment"},
  ignored = {"string", "regex", "comment", "character"},
  ignoredpluschar = {"string", "regex", "comment"},
}

-- Invert regions to get map of primitive to regions.
local primitives = vim.iter(regions):fold({}, function(acc, k, v)
  vim.iter(v):each(function(prim)
    acc[prim] = acc[prim] or {}
    acc[prim][k] = true
  end)
  return acc
end)

-- Map a node type() to corresponding primitive or nil if node not primitive.
-- Assumption: A node cannot be more than one primitive.
---@param typ string
---@return string? key # name of primitive corresponding to input node typ or nil
local function is_primitive(typ)
  -- Return the primitive name or nil if none match.
  return vim.iter(regexes):filter(function (_, v)
    return v:match_str(typ)
  end):next()
end

-- Return true iff at least one of the captures at line/col matches the input pattern.
---@param rgn string # One of the keys in regions[]
---@param line integer # 1-based line number
---@param col integer # 1-based col number
---@return boolean? matched # true/false if definitely in/not in rgn, nil for unknown
function M.is_rgn_type(rgn, line, col)
  --dbg:logf("%d, %d: %s", line, col, rgn)
  -- Note: get_captures_at_pos requires Neovim 0.9. I believe it existed under other names
  -- prior to that, but version-specific logic isn't warranted, given that Neovim
  -- lispers/schemers will almost certainly have something at least that recent.
  if not vim.fn.exists('*vim.treesitter.get_node') then
    return nil
  end
  -- Grab api-indexed pos that supports comparison operators.
  local pos = ApiPos:new(line-1, col-1)
  ---@type string|false|nil
  local ts = vim.fn.reltime()
  local cstat = cache:lookup(pos, regions[rgn])
  prof:add("lookup", vim.fn.reltimestr(vim.fn.reltime(ts)))
  --dbg:logf("Cache lookup took %s !!!!!!!!!!", vim.fn.reltimestr(vim.fn.reltime(ts)))

  if type(cstat) == "string" then
    -- Cache hit! But does the matching primitive satisfy rgn?
    --dbg:logf("Cache hit: returning %s", primitives[cstat][rgn])
    return primitives[cstat][rgn]
  elseif cstat == false then
    -- Cache miss! Can't match rgn.
    --dbg:logf("Cache miss!")
    return false
  end
  -- Cache not useful. Get the node.
  -- TODO: Probably get_parser (and parse()?) first? Could at least cache whether those
  -- calls are necessary if they're expensive...
  ts = vim.fn.reltime()
  local node = vim.treesitter.get_node({pos = pos:positions()})
  prof:add("get_node", vim.fn.reltimestr(vim.fn.reltime(ts)))
  --dbg:logf("Got node in %s !!!!!", vim.fn.reltimestr(vim.fn.reltime(ts)))
  -- Look for matching primitive.
  if not node then
    -- This really shouldn't happen, since most file types have a top-level node.
    --dbg:logf("No node! Adding miss at %s", pos)
    cache:add_miss(pos)
    return false
  end
  -- See whether node is a primitive.
  local key = is_primitive(node:type())
  if key then
    -- Add as cache hit regardless, but return true only if primitive corresponds to rgn.
    --dbg:logf("Caching %s at %s", key, pos)
    cache:add_hit(node, key)
    --dbg:logf("...match status: %s", primitives[key][rgn])
    return primitives[key][rgn]
  else
    --dbg:logf("No match at %s", pos)
    cache:add_miss(pos)
    return false
  end
end

return M

-- vim:ts=2:sw=2:et:tw=90:ai:si
