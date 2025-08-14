local M = {}
local ApiPos = require'sexp.pos'
local ApiRange = require'sexp.range'

--local dbg = require'dp':get('sexp', {enabled=false})
--local prof = require'sexp.prof'

local reltime = vim.fn.reltime
local reltimefloat = vim.fn.reltimefloat
local reltimestr = vim.fn.reltimestr

local bracket = [[\v\(|\)|\[|\]|\{|\}]]
local delimiter = bracket .. [[|\s]]
local re_delimiter = vim.regex(delimiter)

-- Lua patterns for opening/closing brackets
local opening_bracket = [[\v\(|\[|\{]]
local closing_bracket = [[\v\)|\]|\}]]
-- Lua pattern used to check whether a TSNode looks like a list.
-- TODO: Decide whether this is safe for all targeted lisps; if not, we'll need more
-- complex test.
local list_node_patt = [[list]]

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

function M.show_cache()
  cache:show()
end

---@param line integer # 1-based line number
---@param col integer # 1-based col number
---@return TSNode? node # node at line, col, else nil
function M.get_node(line, col)
  local pos = ApiPos:new(line-1, col-1)
  -- TODO: Another cache function get_node() that returns only node?
  local _, node = cache:lookup(pos, true)
  return node
end

---@return TSNode?
function M.get_root()
  return cache:get_root(true)
end

-- Determine whether virtual region key matches at input line/col, returning a multi (see
-- annotation).
---@param rgn string # One of the keys in regions[]
---@param line integer # 1-based line number
---@param col integer # 1-based col number
---@return boolean? # true/false if matched/unmatched, nil for unknown
---@return TSNode? # the matching node, else nil
---@return string? # the matching primitive, else nil
function M.is_rgn_type(rgn, line, col)
  --dbg:logf("is_rgn_type(%d, %d: %s", line, col, rgn)
  -- Grab api-indexed pos that supports comparison operators.
  local pos = ApiPos:new(line-1, col-1)
  local ts = reltime()
  local key, node = cache:lookup(pos, true)
  --dbg:logf("cache:lookup: pos=%s, node=%s", pos, vim.inspect(node))
  local tsf = reltimefloat(reltime(ts))

  if key then
    -- Cache hit!
    --dbg:logf("found cached %s", key)
    --prof:add(string.format("Cache hit: %s", key), tsf)
    -- Cache hit! But does the matching primitive satisfy rgn?
    --dbg:logf("Cache %s", primitives[key][rgn] and "hit" or "miss")
    -- Caveat: First return mustn't be nil.
    return primitives[key][rgn] or false, node, key
  end
  -- No cache hit, but do we have a node?
  if not node then
    --dbg:logf("Ooops! No tree!")
    return nil
  end
  -- See whether node is a primitive.
  key = is_primitive(node:type())
  if key then
    -- Add as cache hit unconditionally, returning true only if primitive corresponds to rgn.
    --dbg:logf("Caching %s at %s", key, pos)
    cache:add_hit(node, key)
    --dbg:logf("No cache: %s %s %s", key, primitives[key][rgn] and "==" or "!=", rgn)
    -- Design Decision: Return node and key regardless of match: caller can ignore.
    return primitives[key][rgn] or false, node, key
  else
    --dbg:logf("No primitive at %s", pos)
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
-- Note: In keeping with legacy behavior, only comments on adjacent lines are combined:
-- i.e., sequences of comments separated by blank lines are distinct elements.
---@param node TSNode
---@param dir 0|1
---@return TSNode
local function find_last_comment_node(node, dir)
  local fwd = dir == 1
  local sr, _, er, _ = ApiRange:from_node(node):to_vim_positions()
  while true do
    local n = node[fwd and 'next_named_sibling' or 'prev_named_sibling'](node)
    if n and is_primitive(n:type()) == 'comment' then
      local sr_, _, er_, _ = ApiRange:from_node(n):to_vim_positions()
      if fwd and er + 1 < sr_ or not fwd and er_ + 1 < sr then
        -- End comment.
        break
      end
      -- Accumulate the node.
      node = n
      sr, _, er, _ = ApiRange:from_node(node):to_vim_positions()
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
  local _, line, col = unpack(vim.fn.getpos('.'))
  local m, node, key = M.is_rgn_type(rgn, line, col)
  if m and node and key then
    -- Current node is of desired rgn type; find its end.
    -- Special Case: Treat run of line comments (each of which is distinct Treesitter
    -- node) as a single unit, provided they're not separated by blank lines.
    if key == 'comment' then
      node = find_last_comment_node(node, dir)
    end
    --local sr, sc, er, ec = ApiRange:from_node(node):to_positions()
    --return dir == 1 and {0, er, ec, 0} or {0, sr, sc, 0}
    --[[
    dbg:logf("current_region_terminal returning %s",
      dir == 1 and ApiPos:new(node:end_()) or ApiPos:new(node:start()))
      ]]
    return dir == 1 and ApiPos:new(node:end_()):to_vim4(true) or ApiPos:new(node:start()):to_vim4()
  else
    -- Caller is responsible for ensuring this doesn't happen!
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
        -- FIXME: Remove this in favor of ApiRange methods.
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

-- See description of sexp#super_range.
---@param beg VimPos
---@param end_ VimPos
---@return [VimPos, VimPos]?
function M.super_range(beg, end_)
  local save_curpos = vim.fn.getcurpos()
  --dbg:logf("super_range(%s, %s)", vim.inspect(beg), vim.inspect(end_))
  -- In case of upwards traversal, this tuple will hold the found start/end nodes.
  ---@type [TSNode?, TSNode?]
  local nodes_ = {}
  -- Short-circuit optimization: skip upwards traversals for common case of single-char region.
  -- Rationale: Think vie.
  if beg[2] ~= end_[2] or beg[3] ~= end_[3] then
    -- Grab the active treesitter root.
    local root = cache:get_root(true)
    if not root then return nil end
    -- Grab nodes containing both ends of initial range.
    local s = ApiPos:from_vim4(beg)
    local e, e_ = ApiPos:from_vim4(end_), ApiPos:from_vim4(end_, true)
    local snode = root:named_descendant_for_range(s.r, s.c, s.r, s.c)
    -- Note: Could use e or e_ for end of range.
    local enode = root:named_descendant_for_range(e.r, e.c, e_.r, e_.c)
    if not snode or not enode then
      --dbg:logf("Returning nil: snode=%s enode=%s", vim.inspect(snode), vim.inspect(enode))
      return nil
    end
    --dbg:logf("Getting container node")
    -- Get node that contains *both* start and end.
    -- Note: Important to use e_ (not e) for end of rnage
    local ct_node = root:named_descendant_for_range(s.r, s.c, e_.r, e_.c)
    -- Loop upward from both start and end nodes until the containing node is reached.
    -- Note: Default terminals are beg/end_, with an upwards adjustment upon each loop
    -- iteration; thus, initial positions are unchanged if loop not entered.
    ---@type [TSNode, TSNode]
    local nodes = {snode, enode}
    for i = 1, 2 do
      ---@type TSNode? # starting point for upwards traversal
      local n = nodes[i]
      while n and n ~= ct_node do
        nodes_[i] = n
        n = n:parent()
      end
    end
  end
  -- Initialize return positions, accounting for any upwards movement in loops above.
  local svp = nodes_[1] and ApiPos:new(nodes_[1]:start()):to_vim4() or vim.list_slice(beg)
  local evp = nodes_[2] and ApiPos:new(nodes_[2]:end_()):to_vim4(true) or vim.list_slice(end_)
  --dbg:logf("---------------------")
  --dbg:logf("svp=%d,%d evp=%d,%d", svp[2], svp[3], evp[2], evp[3])
  -- Make sure start and end do not contain *partial* elements.
  vim.fn.setpos('.', svp)
  --dbg:logf("Finding current_element_terminal(0) for svp: %s", vim.inspect(svp))
  ---@type [VimPos4, VimPos4]
  local p = vim.fn['sexp#current_element_terminal'](0)
  if p[2] ~= 0 then svp = p end
  --dbg:logf("Found current_element_terminal(0) for svp: %s", vim.inspect(svp))
  vim.fn.setpos('.', evp)
  --dbg:logf("Finding current_element_terminal(1) for evp: %s", vim.inspect(evp))
  p = vim.fn['sexp#current_element_terminal'](1)
  if p[2] ~= 0 then evp = p end
  --dbg:logf("Found current_element_terminal(1) for evp: %s", vim.inspect(evp))
  -- Restore position.
  vim.fn.setpos('.', save_curpos)
  -- At this point, snode and enode could be the same or different nodes. It's also
  -- possible one or both are not within an element (i.e., blank or whitespace).
  --dbg:logf("super_range returning %s-%s", vim.inspect(svp), vim.inspect(evp))
  return {svp, evp}
end

-- Return VimPos4 representing the position of nearest containing bracket of specified type.
-- Cursor Positioning: Legacy version of this function leaves cursor on found bracket, so
-- we do too.
---@param closing boolean # true to look forward for closing bracket
---@param open_re string? # Regex corresponding to open bracket, nil for default
---@param close_re string? # Regex corresponding to close bracket, nil for default
---@return [boolean, VimPos4|string] # success flag, followed by location of bracket, else error string
function M.nearest_bracket(closing, open_re, close_re)
  -- Get cursor pos as ApiIndex.
  local pos = ApiPos:from_vim4(vim.fn.getpos('.'))
  -- Grab the active Treesitter root.
  local root = M.get_root()
  if not root then
    return {false, "Unable to get Treesitter root for buffer"}
  end
  local row, col = pos:positions()
  -- Convert pos to an exclusive range containing a single char.
  ---@type TSNode?
  local node = root:named_descendant_for_range(row, col, row, col + 1)
  if not node then
    return {false, "Unable to get Treesitter node"}
  end
  -- Get pattern used to determine whether start/end of range is desired bracket.
  local bracket_re = closing and (close_re or closing_bracket) or (open_re or opening_bracket)
  -- Look upwards for containing bracket of desired type in indicated direction.
  while node do
    -- Check the current node.
    local node_type = node:type()
    -- TODO: Decide whether this test is a reliable indication of a list node across the various
    -- lisps; if not, we may need another type check to ensure a bracket is not within
    -- string/regex/comment/etc...
    if node_type:find(list_node_patt) then
      -- Looks promising, but does the node start/end with correct bracket?
      -- TODO: Consider enhancing ApiPos to simplify the following.
      ---@type integer
      local r, c
      if closing then
        r, c = node:end_()
        c = c - 1 -- make inclusive
      else
        r, c = node:start()
      end
      -- TODO: Determine whether this is faster than getline() (for typical line lengths)
      -- and switch to getline() if not.
      -- Note: nvim_buf_get_text() returns empty array on empty buffer, in which case the
      -- table index of 1 will extract nil.
      ---@type string
      local ch = vim.api.nvim_buf_get_text(0, r, c, r, c + 1, {})[1]
      if not ch then
        -- TODO: This shouldn't happen. Can it even, given that node is non-nil?
        return {false, "Internal error: Unable to obtain buffer text"}
      end
      if vim.fn.match(ch, bracket_re) >= 0 then
        -- Found desired bracket! Return inclusive ApiPos as VimPos4.
        return {true, {0, r + 1, c + 1, 0}}
      else
        -- Found a containing list with incorrect bracket type.
        -- TODO: How should this be handled? For now, just give up and let legacy logic handle.
        -- I don't think this should ever happen.
        return {false, "Wrong bracket type"}
      end
    end
    -- Keep looking upwards till we find what we're seeking or hit root...
    node = node:parent()
  end
  -- Reached toplevel without finding containing list.
  return {false, "Not found"}
end

return M

-- vim:ts=2:sw=2:et:tw=90:ai:si
