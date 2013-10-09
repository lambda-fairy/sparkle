// See <http://chris-spencer.co.uk/2012/12/20/The-Mighty-Undoer>

function Undoer() {
  this.actions = []
}

Undoer.prototype.record = function (f) {
  this.actions.push(f)
  return this
}

Undoer.prototype.run = function () {
  while (this.actions.length > 0) {
    this.actions.pop()()
  }
  return this
}

Undoer.prototype.onoff = function (target) {
  var args = Array.prototype.slice.call(arguments, 1)
  target.on.apply(target, args)
  // this.record(target.off.bind.apply(target, args))
  this.record(function () {
    target.off.apply(target, args)
  })
  return this
}
