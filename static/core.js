// Firefox doesn't support assert for some reason.  Pickle barrel kumquat.
console.assert = console.assert || function () {}

jQuery.fn.extend({
  isBlank: function () {
    return /^\s*$/.test(this.text())
  },
  fuzzyCheckboxes: function (selector) {
    this.on('click', selector, function () {
      $(this).find(':checkbox').click()
    })
    this.on('click', selector + ' :checkbox', function (e) {
      e.stopPropagation()
    })
    return this
  }
})

var Sparkle = (function ($) { 'use strict';

  var Server = {
    getPlan: function () {
      return $.get('/', {plain: true})
    },
    insertTask: function (id, data) {
      data = data || emptyTask
      return $.post('/api/v0/tasks/'+id+'/new', JSON.stringify(data))
    },
    modifyTask: function (id, data) {
      return $.post('/api/v0/tasks/'+id, JSON.stringify(data))
    },
    deleteTask: function (id) {
      return $.ajax({
        type: 'DELETE',
        url: '/api/v0/tasks/'+id
      })
    }
  }

  var emptyTask = {done: false, title: ''}

  function Sparkle(rootSelector) {
    this.$root = $(rootSelector)
    this.$root.fuzzyCheckboxes('.task-done')

    this.u = new Undoer()
  }

  Sparkle.prototype.allTasks = function () {
    return this.$root.find('.task')
  }

  Sparkle.prototype.getTaskById = function (id) {
    return this.allTasks().filter(function () {
      return $(this).data('id') == id
    })
  }

  Sparkle.prototype._transition = function (newState) {
    var oldState = this.state
    if (newState !== oldState) {
      console.log('Transitioning from %s -> %s', oldState, newState)
      this.state = newState
    }

    // Roll back the previous state
    this.u.run()

    // Set CSS classes for styling
    this.$root.removeClass(oldState).addClass(newState)

    // Enable/disable input fields
    this.$root.find('input').prop('disabled', newState === 'locked')
  }

  Sparkle.prototype.switchIdle = function () {
    this._transition('idle')

    var thisObj = this
    this.u.onoff(this.$root, 'focus', '.task-title', function () {
      var $task = $(this).closest('.task')
      thisObj.switchEditing($task)
    }).onoff(this.$root, 'change', '.task-done :checkbox', function () {
      var $task = $(this).closest('.task')
      thisObj.saveTask(new Task($task))
    })
  }

  Sparkle.prototype.switchEditing = function ($task) {
    this._transition('editing')

    // Plan can't reload while user is editing a task
    this.cancelReload()

    // Make a box thingy
    this.task = new Task($task)
    this.task.edit()

    var thisObj = this
    this.u.onoff($task, 'blur', '.task-title', function () {
      // When user clicks outside task, save changes
      thisObj.saveTask(thisObj.task)
    }).onoff($task, 'keydown', '.task-title', function (e) {
      var task = thisObj.task
      var sel = document.getSelection()
      var cursor = sel.rangeCount === 1 && sel.getRangeAt(0)
      var cursorAtStart = isCursorAtStart(task.$title, cursor)
      var cursorAtEnd = isCursorAtEnd(task.$title, cursor)

      if (e.which === 8) {
        // <Backspace>
        if (task.id > 0 && cursorAtStart) {
          // If cursor at beginning of line, merge contents with
          // previous task
          thisObj.deleteTask(task).then(function () {
            var $prev = thisObj.getTaskById(task.id - 1)
            var prevTask = thisObj.switchEditing($prev)

            // Make sure the cursor is in the right place
            var newPos = (function () {
              var r = document.createRange()
              r.selectNodeContents(prevTask.$title.get(0))
              r.collapse(false)
              return r
            })()
            sel.removeAllRanges(); sel.addRange(newPos)

            // Merge
            prevTask.$title.append(task.$title.html())
          })
        }
      } else if (e.which === 46) {
        // <Del>
        if (task.id < thisObj.allTasks().length - 1 && cursorAtEnd) {
          // If cursor at end of line, merge contents with next task
          thisObj.deleteTask(task).then(function () {
            var $next = thisObj.getTaskById(task.id)
            var nextTask = thisObj.switchEditing($next)
            nextTask.$title.prepend(task.$title.html())
          })
        }
      } else if (e.which === 27) {
        // <Esc>
        thisObj.saveTask(task)
      } else if (e.which === 13 && !e.shiftKey) {
        // <Return>
        var newData
        var offset
        if (cursorAtStart || !cursor || !cursor.collapsed) {
          // If cursor at beginning of line, create a new blank task
          // before the current one.
          newData = null
          offset = 0
        } else {
          // Else, split the current task at the cursor.  Place the
          // latter half into a new task after the current one.
          var remainder = (function () {
            var r = document.createRange()
            r.selectNodeContents(task.$title.get(0))
            r.setStart(cursor.startContainer, cursor.startOffset)
            return r.extractContents().textContent
          })()
          newData = emptyTask; newData['title'] = remainder
          offset = 1
        }
        thisObj.saveTask_(task).then(thisObj.newTask.bind(thisObj, offset + task.id, newData))
      }
    })

    return this.task
  }

  function isCursorAtStart($parent, r) {
    if ($parent.isBlank())
      return true

    if (!r || !r.collapsed)
      return false

    var node = r.commonAncestorContainer
    return $parent.get(0).firstChild === node &&
      node.nodeType === document.TEXT_NODE &&
      r.startOffset === 0
  }

  function isCursorAtEnd($parent, r) {
    if ($parent.isBlank())
      return true

    if (!r.collapsed)
      return false

    var node = r.commonAncestorContainer
    return $parent.get(0).lastChild === node &&
      node.nodeType === document.TEXT_NODE &&
      r.startOffset === node.nodeValue.length
  }

  Sparkle.prototype.deleteTask = function (task) {
    this.switchLocked()
    return task.delete_().then(this.reload.bind(this))
  }

  Sparkle.prototype.saveTask_ = function (task) {
    console.assert(task, 'The task being edited cannot be null')

    // The user shouldn't be able to make changes while it's saving
    this.switchLocked()

    // Send the request
    var thisObj = this
    return task.save().fail(function () {
      thisObj.connectionLost()
    })
  }

  Sparkle.prototype.saveTask = function (task) {
    return this.saveTask_(task).then(this.reload.bind(this))
  }

  Sparkle.prototype.newTask = function (newId, newData) {
    var thisObj = this
    return Server.insertTask(newId, newData).then(this.reload.bind(this)).done(function () {
      thisObj.switchEditing(thisObj.getTaskById(newId))
    })
  }

  Sparkle.prototype.switchLocked = function () {
    this._transition('locked')
  }

  // Schedule a reload of the plan.
  Sparkle.prototype.reload = function () {
    console.log('Reloading plan')

    this.cancelReload()

    var thisObj = this
    this._reloadDeferred = Server.getPlan().done(function (data) {
      console.assert(
        thisObj.state !== 'editing',
        'The plan should never reload while the user is editing it')
      thisObj.$root.html(data)
      thisObj.switchIdle()
      thisObj._reloadTimer = setTimeout(function () { thisObj.reload() }, 5000)
    }).fail(function (ajax) {
      if (ajax.statusText !== 'abort') {
        // This failed because of an actual error, not cancelReload()
        thisObj.connectionLost()
      }
    })

    return this._reloadDeferred
  }

  // Cancel a reload in progress.
  Sparkle.prototype.cancelReload = function () {
    if (this._reloadDeferred) {
      this._reloadDeferred.abort()
      delete this._reloadDeferred
    }
    if (this._reloadTimer) {
      clearTimeout(this._reloadTimer)
      delete this._reloadTimer
    }
  }

  Sparkle.prototype.connectionLost = function () {
    this.switchLocked()
    alert('Connection lost. Reload the page and try again.')
  }

  function Task($task) {
    this.id = $task.data('id')
    this.$task = $task
    this.$done = $task.find('.task-done')
    this.$title = $task.find('.task-title')
  }

  Task.prototype.edit = function () {
    console.log('Editing task <%s>', this.id)
    this.$title.attr('contenteditable', 'true').focus()
  }

  Task.prototype.delete_ = function () {
    console.log('Deleting task <%s>', this.id)
    return Server.deleteTask(this.id)
  }

  Task.prototype.save = function () {
    console.log('Saving task <%s>', this.id)

    var done_ = this.$done.find(':checkbox').prop('checked')
    var title_ = this.$title.text()

    // Make task read-only again
    this.$title.removeAttr('contenteditable')

    // Send it awayways
    return Server.modifyTask(this.id, {done: done_, title: title_})
  }

  return Sparkle

})(jQuery)

jQuery(function () {
  window.S = new Sparkle('#plan')
  S.reload()
})
