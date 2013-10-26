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
    var isLocked = newState === 'locked'
    this.$root.find('input').prop('disabled', isLocked)
    if (isLocked)
      this.$root.find('.task-title').removeAttr('tabindex')
    else
      this.$root.find('.task-title').attr('tabindex', -1)
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

  Sparkle.prototype.switchEditing = function ($task, moveCursorToEnd) {
    this._transition('editing')

    // Plan can't reload while user is editing a task
    this.cancelReload()

    // Make a box thingy
    this.task = new Task($task)
    this.task.edit(moveCursorToEnd)

    var thisObj = this
    this.u.onoff($task, 'blur', '.task-title', function () {
      // When user clicks outside task, save changes
      thisObj.saveTask(thisObj.task)
    }).onoff($task, 'keydown', '.task-title', function (e) {
      var task = thisObj.task
      var isBlank = task.$title.isBlank()
      var nTasks = thisObj.allTasks().length
      if (e.which === 8 && isBlank && task.id > 0) {
        // <Backspace>
        thisObj.deleteTask(task).then(function () {
          var $tasks = thisObj.allTasks()
          var next = $tasks[task.id - 1] || $tasks[task.id]
          if (next)
            thisObj.switchEditing($(next), true)
        })
      } else if (e.which === 46 && isBlank && task.id < nTasks-1) {
        // <Del>
        thisObj.deleteTask(task).then(function () {
          var $tasks = thisObj.allTasks()
          var next = $tasks[task.id] || $tasks[task.id - 1]
          if (next)
            thisObj.switchEditing($(next), true)
        })
      } else if (e.which === 46 && e.shiftKey) {
        // <Shift-Del>
        thisObj.deleteTask(task)
      } else if (e.which === 27) {
        // <Esc>
        thisObj.saveTask(task)
      } else if (e.which === 13 && !e.shiftKey) {
        // <Return>
        thisObj.saveTask_(task).then(thisObj.newTask.bind(thisObj, 1 + task.id))
      }
    })

    return this.task
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
      var next = thisObj.allTasks()[newId]
      if (next)
        thisObj.switchEditing($(next))
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
      thisObj._replacePlan(data)
      thisObj._reloadTimer = setTimeout(function () { thisObj.reload() }, 5000)
    }).fail(function (ajax) {
      if (ajax.statusText !== 'abort') {
        // This failed because of an actual error, not cancelReload()
        thisObj.connectionLost()
      }
    })

    return this._reloadDeferred
  }

  Sparkle.prototype._replacePlan = function (data) {
    var thisObj = this
    var $root = this.$root
    $root.html(data)
    $root.find('.task-done').css('user-select', 'none')
    $root.find('.task').each(function () {
      var $task = $(this)
      var $delete = $('<td>').addClass('task-delete')
      $delete.click(function () {
        thisObj.deleteTask(new Task($task))
      })
      $task.find('.task-title').after($delete)
    })

    this.switchIdle()

    var $add = $('<a>').attr('href', 'javascript:void 0').addClass('add-task').text('+ add task')
    $add.click(function () {
      thisObj.newTask(thisObj.allTasks().length)
    })
    $root.append($add)
    this.u.record(function () {
      $add.remove()
    })
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

  Task.prototype.edit = function (moveCursorToEnd) {
    console.log('Editing task <%s>', this.id)
    this.$title.attr('contenteditable', 'true').focus()

    if (moveCursorToEnd) {
      // Move cursor to end of text field
      var sel = document.getSelection()
      var r = document.createRange()
      r.selectNodeContents(this.$title.get(0))
      r.collapse(false)
      sel.removeAllRanges(); sel.addRange(r)
    }
  }

  Task.prototype.delete_ = function () {
    console.log('Deleting task <%s>', this.id)
    return Server.deleteTask(this.id)
  }

  Task.prototype.save = function () {
    console.log('Saving task <%s>', this.id)

    var done_ = this.$done.find(':checkbox').prop('checked')
    var title_ = this.$title.text().replace(/\s+$/, '')

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
