local M = {}
local ApiPos = require'sexp.pos'

local dbg = require'dp':get('sexp', {enabled=true})
local prof = require'sexp.prof'

local reltime = vim.fn.reltime
local reltimefloat = vim.fn.reltimefloat
local reltimestr = vim.fn.reltimestr

local bracket = [[\v\(|\)|\[|\]|\{|\}]]
local delimiter = bracket .. [[|\s]]
local re_delimiter = vim.regex(delimiter)

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

-- Map of region keys to primitives
-- Note: Answers the question, Which primitives satisfy this region?
-- Note: A region key is a sort of virtual name for a set of primitives.
local regions = {
  string = {"string"},
  comment = {"comment"},
  -- Consider renaming these string_comment_char string_comment or str_com_chr str_com
  str_com_chr = {"string", "regex", "comment", "character"},
  str_com = {"string", "regex", "comment"},
}

-- Invert regions map to get map of primitives to regions.
-- Note: Answers the question, Which regions does this primitive satisfy?
local primitives = vim.iter(regions):fold({}, function(acc, k, v)
  vim.iter(v):each(function(prim)
    acc[prim] = acc[prim] or {}
    acc[prim][k] = true
  end)
  return acc
end)

-- Map a node type() to corresponding primitive or nil if node not primitive.
-- Assumption: A node cannot be more than one primitive.
---@param typ string # node:type()
---@return string? key # name of primitive corresponding to input node typ or nil
local function is_primitive(typ)
  -- Return the primitive name or nil if none match.
  return vim.iter(regexes):filter(function (_, v)
    return v:match_str(typ)
  end):next()
end

-- Return true iff input node matches named region.
---@param node TSNode
---@param rgn string # must be one of the keys in regions
---@return boolean # true iff node matches specfied region
local function is_node_rgn_type(node, rgn)
  local typ = node:type()
  return vim.iter(regions[rgn]):any(function (prim) return regexes[prim]:match_str(typ) end)
end

function M:show_cache()
  cache:show()
end

-- Return true (and matched node) iff region key matches at input line/col.
---@param rgn string # One of the keys in regions[]
---@param line integer # 1-based line number
---@param col integer # 1-based col number
---@return boolean? # true/false if matched/unmatched, nil for unknown
---@return TSNode? # the matching node, else nil
---@return string? # the matching primitive, else nil
function M.is_rgn_type(rgn, line, col)
  dbg:logf("is_rgn_type(%d, %d: %s", line, col, rgn)
  -- Grab api-indexed pos that supports comparison operators.
  local pos = ApiPos:new(line-1, col-1)
  local ts = reltime()
  local key, node = cache:lookup(pos, true)
  dbg:logf("cache:lookup: pos=%s, node=%s", pos, vim.inspect(node))
  local tsf = reltimefloat(reltime(ts))

  if key then
    -- Cache hit!
    dbg:logf("found cached %s", key)
    prof:add(string.format("Cache hit: %s", key), tsf)
    -- Cache hit! But does the matching primitive satisfy rgn?
    dbg:logf("Cache %s", primitives[key][rgn] and "hit" or "miss")
    -- Caveat: First return mustn't be nil.
    return primitives[key][rgn] or false, node, key
  end
  -- No cache hit, but do we have a node?
  if not node then
    dbg:logf("Ooops! No tree!")
    return nil
  end
  -- See whether node is a primitive.
  key = is_primitive(node:type())
  if key then
    -- Add as cache hit unconditionally, returning true only if primitive corresponds to rgn.
    --dbg:logf("Caching %s at %s", key, pos)
    cache:add_hit(node, key)
    dbg:logf("No cache: %s %s %s", key, primitives[key][rgn] and "==" or "!=", rgn)
    -- Design Decision: Return node and key regardless of match: caller can ignore.
    return primitives[key][rgn] or false, node, key
  else
    dbg:logf("No primitive at %s", pos)
    return false
  end
end

-- Perform any special adjustments required to go from [0,0) TSNode indexing to [1,1].
-- Rationale: Some nodes end in column 0 of following line.
-- TODO: Consider a wrapper class that provides range methods for api, mark-like and vim
-- indexing.
-- Alternatively, make this a conversion function in a range.lua utility module.
---@param node TSNode
---@return integer
---@return integer
---@return integer
---@return integer
local function convert_node_range(node)
  local sr, sc, er, ec = node:range()
  sr, er = sr+1, er+1
  -- Adjust start col
  sc = sc + 1
  -- Note: [0,0) col position is already inclusive in a [1,1] sense, except when ec is
  -- BOL, a case which requires special handling.
  if ec == 0 then
    -- Convert exclusive end at SOL to inclusive end at EOL of previous line.
    er = er - 1
    ec = vim.fn.col({er, '$'}) - 1
  end
  return sr, sc, er, ec
end

-- From input node, find last of consecutive comment nodes in specified direction,
-- starting with input.
---@param node TSNode
---@param dir 0|1
---@return TSNode
local function find_last_comment_node(node, dir)
  local fwd = dir == 1
  while true do
    local n = node[fwd and 'next_named_sibling' or 'prev_named_sibling'](node)
    if n and is_primitive(n:type()) == 'comment' then
      local sr, _, er, _ = convert_node_range(node)
      local sr_, _, er_, _ = convert_node_range(n)
      if fwd and er + 1 < sr_ or not fwd and er_ + 1 < sr then
        -- Design Decision: For now, keep legacy behavior by not combining comments
        -- that aren't on adjacent lines.
        break
      end
      node = n
    else
      break
    end
  end
  return node
end

-- Assumption: Rgn type corresponds to one of the primitives.
---@param rgn string # One of the keys in regions[]
---@param dir 0|1
---@return [integer, integer, integer, integer]?
function M.current_region_terminal(rgn, dir)
  local pos = vim.api.nvim_win_get_cursor(0)
  local line, col = pos[1], pos[2] + 1
  local m, node, key = M.is_rgn_type(rgn, line, col)
  if m and node and key then
    if key == 'comment' then
      node = find_last_comment_node(node, dir)
    end
    local sr, sc, er, ec = convert_node_range(node)
    return dir == 1 and {0, er, ec, 0} or {0, sr, sc, 0}
  else
    return {0, 0, 0, 0}
  end
end

-- FIXME: Don't like the redundancy with current_atom_terminal, but efficiency is
-- important. Is this needed elsewhere?
function M.is_atom(line, col)
  local linetext = vim.fn.getline(line)
  if #linetext == 0 then
    return false
  elseif re_delimiter:match_str(linetext:sub(col, col+1)) and not M.is_rgn_type('str_com_chr', line, col) then
    return false
  end
  --local node = vim.treesitter.get_node({pos = {line-1, col-1}})
  return M.is_rgn_type('str_com', line, col)
end

---@param dir 0|1
---@return [integer, integer, integer, integer]
function M.current_atom_terminal(dir)
  local pos = vim.api.nvim_win_get_cursor(0)
  local line, col = pos[1], pos[2] + 1
  local fwd = dir == 1
  -- Note: If termcol is still 0 at return, return null pos.
  local termcol = 0
  local linetext = vim.fn.getline(line)
  -- Blank line can't contain atom.
  if #linetext == 0 then return {0, 0, 0, 0} end
  -- Use nearest whitespace on current line in desired direction as boundary.
  -- If no such whitespace, leave limcol nil to use BOL and EOL.
  local limit = vim.fn.searchpos([[\v\s]], fwd and 'nW' or 'nbW', line)
  ---@type integer?
  local limcol = limit[2] > 0 and limit[2] or nil
  -- Loop over bytes starting at cursor, adjusting termcol as atom chars are verified.
  ---@type integer
  local eol = vim.fn.col({line, '$'})
  while fwd and col < (limcol or eol) or not fwd and col > (limcol or 0) do
    -- Check col position.
    --dbg:logf("Checking %s at %d, %d", linetext:sub(col, col), line, col)
    if re_delimiter:match_str(linetext:sub(col, col)) and not M.is_rgn_type('str_com_chr', line, col) then
      -- Unignored bracket is not part of atom.
      break
    end
    -- Get ts node at current position.
    --dbg:logf("limcol: %s", limcol)
    local node = vim.treesitter.get_node({pos = {line-1, col-1}})
    if node and not is_node_rgn_type(node, 'str_com') then
      -- Still within atom. If node is leaf, skip to its end (limit permitting).
      if node:child_count() == 0 then
        -- Get [1,1] indexed pos.
        local sr, sc, er, ec = convert_node_range(node)
        --dbg:logf("Leaf! %d, %d, %d, %d", sr, sc, er, ec)
        if fwd and er ~= line or not fwd and sr ~= line then
          -- This really shouldn't happen; use whitespace as limit.
          termcol = fwd and (limcol and limcol - 1 or eol - 1) --[[@as integer]] or
            not fwd and (limcol and limcol + 1 or 1)
            --dbg:logf("Breaking in the off-nominal case termcol=%d", termcol)
          break
        else
          -- Use node terminal as limit, provided it's within whitespace limit.
          termcol = fwd and math.min(limcol and limcol-1 or eol-1, ec) --[[@as integer]]
            or not fwd and math.max(limcol and limcol+1 or 1, sc)
        end
      else
        -- Need to keep checking col-by-col, since a non-leaf node can contain a child
        -- that starts a new region at any point.
        termcol = col
      end
      -- Note: Don't worry about redundant iterations in multi-byte chars.
      col = termcol + (fwd and 1 or -1)
      --dbg:logf("Updated col to %d", col)
    else
      --dbg:logf("Breaking because no node termcol=%d col=%d", termcol, col)
      break
    end
  end
  return {0, termcol ~= 0 and line or 0, termcol, 0}
end

return M

-- vim:ts=2:sw=2:et:tw=90:ai:si
