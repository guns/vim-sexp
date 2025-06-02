local prof = require'sexp.prof'

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
---@return ApiPos
---@return ApiPos
function ApiPos:range_of_tsnode(node)
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

---@return integer[]
function ApiPos:positions4()
  return {0, self.row, self.col, 0}
end

function ApiPos:__tostring()
  -- FIXME
  local ts = vim.fn.reltime()
  local ret = "[" .. self.row .. ", " .. self.col .. "]"
  prof:add("__tostring", vim.fn.reltimestr(vim.fn.reltime(ts)))
  return ret
end

return ApiPos

-- vim:ts=2:sw=2:et:tw=90:ai:si
