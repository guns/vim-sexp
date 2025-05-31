local M = {}
M.__index = M

local dbg = require'dp':get('sexp', {enabled=true})

function M:new()
  local o = {
    keys = {}
  }
  return setmetatable(o, M)
end

function M:add(key, ts)
  local o = self.keys[key]
  if not o then
    o = {tot = 0, cnt = 0}
    self.keys[key] = o
  end

  o.tot = o.tot + ts
  o.cnt = o.cnt + 1
end

function M:clear()
  self.keys = {}
end

function M:show()
  for k, v in pairs(self.keys) do
    dbg:logf("%s: avg=%f cnt=%d", k, v.tot / v.cnt, v.cnt)
  end
end

-- Singleton
return M:new()
-- vim:ts=2:sw=2:et:tw=90:ai:si


