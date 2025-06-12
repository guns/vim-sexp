local prof = require'sexp.prof'

---@alias VimPos2 [integer, integer]
---@alias VimPos4 [integer, integer, integer, integer]
---@alias VimPos VimPos4

---@class ApiPos
---@field r integer # 0-based row
---@field c integer # 0-based col, exclusive if end
local ApiPos = {}
ApiPos.__index = ApiPos

---@param r integer?
---@param c integer?
function ApiPos:new(r, c)
  local o = {r = r or 0, c = c or 0}
  return setmetatable(o, ApiPos)
end

function ApiPos.vim_to_api(r, c, is_end)
  -- Ensure inclusive->exclusive conversion is multi-byte safe.
  return r-1, is_end and c-1 + vim.fn['sexp#char_bytes']({0, r, c, 0}) or c-1
end

-- TODO: Consider caching the vim position for speed; doing so might obviate the need for
-- subsequent conversions.
---@param p VimPos2
---@param is_end boolean?
function ApiPos:from_vim2(p, is_end)
  -- Ensure inclusive->exclusive conversion is multi-byte safe.
  return ApiPos:new(ApiPos.vim_to_api(p[1], p[2], is_end))
end

---@param p VimPos4
---@param is_end boolean?
function ApiPos:from_vim4(p, is_end)
  -- Ensure inclusive->exclusive conversion is multi-byte safe.
  return ApiPos:new(ApiPos.vim_to_api(p[2], p[3], is_end))
end
ApiPos.from_vim = ApiPos.from_vim4

---@param rhs ApiPos
function ApiPos:__lt(rhs)
  return self.r < rhs.r or self.r == rhs.r and self.c < rhs.c
end

---@param rhs ApiPos
function ApiPos:__eq(rhs)
  return self.r == rhs.r and self.c == rhs.c
end

---@return integer, integer
function ApiPos:positions()
  return self.r, self.c
end

-- If input end pos is BOL, adjust to EOL of previous line.
-- Design Decision: Leaving non-BOL alone accomplishes the 0-based exclusive to 1-based
-- inclusive conversion naturally, with the caveat that the 1-based position will be the
-- *end* of a multi-byte char (probably what we want).
---@param is_end boolean?
---@return integer, integer
function ApiPos:vim_adjust(is_end)
  if is_end and self.c == 0 then
    -- Convert exclusive end at SOL to inclusive end at EOL of previous line.
    -- Note: Row is unchanged because the indexing and line pullback adjustments cancel.
    return self.r, vim.fn.col({self.r, '$'}) - 1
  else
    -- No special BOL end logic (though may still be normal end).
    -- Note: Col is unchanged in is_end case because [0,0) col position is identical to
    -- the corresponding [1,1] position.
    return self.r + 1, is_end and self.c or self.c + 1
  end
end

-- Design Decision: Providing an is_end arg for the to_vim* methods can obviate the need
-- for creating a range when only the final position is needed.
---@param is_end boolean?
function ApiPos:to_vim2(is_end)
  return {self:vim_adjust(is_end)}
end

---@param is_end boolean?
function ApiPos:to_vim4(is_end)
  local r, c = self:vim_adjust(is_end)
  return {0, r, c, 0}
end
ApiPos.to_vim = ApiPos.to_vim4

function ApiPos:__tostring()
  -- FIXME
  local ts = vim.fn.reltime()
  local ret = "[" .. self.r .. ", " .. self.c .. "]"
  prof:add("__tostring", vim.fn.reltimestr(vim.fn.reltime(ts)))
  return ret
end

return ApiPos

-- vim:ts=2:sw=2:et:tw=90:ai:si
