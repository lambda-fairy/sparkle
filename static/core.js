// Firefox doesn't support assert for some reason.  Pickle barrel kumquat.
console.assert = console.assert || function () {}

jQuery.fn.extend({
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
      data = data || {done: false, title: ''}
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

  function Sparkle(rootSelector) {
    this.$root = $(rootSelector)
    this.$root.fuzzyCheckboxes('.task-done')

    this.u = new Undoer()
  }

  Sparkle.prototype.getTaskById = function (id) {
    return this.$root.find('.task').filter(function () {
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
    this.cursor = new Task($task)
    this.cursor.edit()

    // When user clicks outside task, save changes
    var thisObj = this
    var $taskTitle = $task.find('.task-title')
    this.u.onoff($taskTitle, 'blur', function () {
      thisObj.saveTask(thisObj.cursor)
    }).onoff($taskTitle, 'keydown', function (e) {
      if (e.which === 8 || e.which === 46) {
        // <Backspace> or <Del>
        if (stringIsSpace($taskTitle.text())) {
          thisObj.deleteTask(thisObj.cursor)
        }
      } else if (e.which === 27) {
        // <Esc>
        thisObj.saveTask(thisObj.cursor)
      } else if (e.which === 13 && !e.shiftKey) {
        // <Return>
        var sel = document.getSelection()
        var offset = sel.rangeCount === 1 && isCursorAtEnd($taskTitle, sel.getRangeAt(0)) ? 1 : 0
        thisObj.saveTask_(thisObj.cursor).then(thisObj.newTask.bind(thisObj, offset + thisObj.cursor.id))
      }
    })
  }

  function stringIsSpace(s) {
    return s.match(/^\s*$/) !== null
  }

  function isCursorAtEnd($parent, r) {
    if (stringIsSpace($parent.text()))
      return true

    if (!r.collapsed)
      return false

    var node = r.commonAncestorContainer
    console.log($parent.get(0).lastChild)
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

  Sparkle.prototype.newTask = function (newId) {
    var thisObj = this
    return Server.insertTask(newId).then(this.reload.bind(this)).done(function () {
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
  }

  Task.prototype.edit = function () {
    console.log('Editing task <%s>', this.id)
    this.$task.find('.task-title').attr('contenteditable', 'true').focus()
  }

  Task.prototype.delete_ = function () {
    console.log('Deleting task <%s>', this.id)
    return Server.deleteTask(this.id)
  }

  Task.prototype.save = function () {
    console.log('Saving task <%s>', this.id)

    var done_ = this.$task.find('.task-done :checkbox').prop('checked')
    var $taskTitle = this.$task.find('.task-title')
    var title_ = $taskTitle.text()

    // Make task read-only again
    $taskTitle.removeAttr('contenteditable')

    // Send it awayways
    return Server.modifyTask(this.id, {done: done_, title: title_})
  }

  return Sparkle

})(jQuery)

jQuery(function () {
  window.S = new Sparkle('#plan')
  S.reload()
})
