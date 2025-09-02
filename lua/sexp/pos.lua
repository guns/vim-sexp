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

---@param is_end boolean? # if true, convert exclusive col to inclusive
---@return integer, integer
function ApiPos:positions(is_end)
  return self.r, is_end and self.c - 1 or self.c
end

-- Return ApiPos (either self or new one) that does not end in column 0.
-- Adjustment Logic: If self end pos is BOL (e.g., for end of line comment), adjust to EOL
-- of previous line.
-- Precondition: This method should be called only for ApiPos representing exclusive end.
---@return ApiPos
function ApiPos:canonical_end()
  if self.c == 0 then
    -- Convert exclusive end at SOL to exclusive end at EOL of previous line.
    -- Design Decision: Ensure col >= 1 (in case of empty lines).
    return ApiPos:new(self.r - 1, math.max(1, vim.fn.col({self.r, '$'}) - 1))
  else
    -- No adjustment required
    return self
  end
end

-- Return 1-based row,col indices corresponding to the invocant, making BOL to EOL
-- adjustment if applicable.
---@param is_end boolean?
---@return integer, integer
function ApiPos:vim_adjust(is_end)
  local pos = is_end and self:canonical_end() or self
  -- Note: The col adjustment returns end of multi-byte char (probably what is desired).
  return pos.r + 1, is_end and pos.c or pos.c + 1
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
  return ret
end

return ApiPos

-- vim:ts=2:sw=2:et:tw=90:ai:si
