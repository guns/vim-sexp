local ApiPos = require'sexp.pos'

---@class ApiRange
---@field s ApiPos
---@field e ApiPos
local ApiRange = {}
ApiRange.__index = ApiRange

---@param s ApiPos
---@param e ApiPos
function ApiRange:new(s, e)
  return setmetatable({s = s, e = e}, ApiRange)
end

---@param p ApiPos
---@return boolean # true iff provided position is within range
function ApiRange:contains(p)
  return self.s <= p and self.e > p
end

---@param sr integer
---@param sc integer
---@param er integer
---@param ec integer
---@return ApiRange
function ApiRange:from_positions(sr, sc, er, ec)
  return ApiRange:new(ApiPos:new(sr, sc), ApiPos:new(er, ec))
end

---@param beg VimPos2
function ApiRange:from_vim2(beg, end_)
  return ApiRange:new(ApiPos:from_vim2(beg), ApiPos:from_vim2(end_, true))
end

---@param beg VimPos4
function ApiRange:from_vim4(beg, end_)
  return ApiRange:new(ApiPos:from_vim4(beg), ApiPos:from_vim4(end_, true))
end
ApiRange.from_vim = ApiRange.from_vim4

---@param node TSNode
---@return ApiRange
function ApiRange:from_node(node)
  return ApiRange:from_positions(node:range())
end

---@return integer, integer, integer, integer
function ApiRange:to_positions()
  return self.s.r, self.s.c, self.e.r, self.e.c
end

-- Like to_positions(), but perform adjustments required to go from [0,0) TSNode indexing to [1,1].
-- Rationale: Some nodes end in column 0 of following line.
---@return integer, integer, integer, integer
function ApiRange:to_vim_positions()
  local sr, sc = self.s:vim_adjust()
  local er, ec = self.e:vim_adjust(true)
  return sr, sc, er, ec
end

function ApiRange:to_vim2()
  return {self.s:to_vim2(), self.e:to_vim2(true)}
end

function ApiRange:to_vim4()
  return {self.s:to_vim4(), self.e:to_vim4(true)}
end
ApiRange.to_vim = ApiRange.to_vim4

return ApiRange

-- vim:ts=2:sw=2:et:tw=90:ai:si
