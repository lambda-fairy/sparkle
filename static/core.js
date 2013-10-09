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

  function Sparkle(rootSelector) {
    this.$root = $(rootSelector)
    this.$root.fuzzyCheckboxes('.task-done')

    this.u = new Undoer()
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
      var $taskData = $(this).closest('.task-data')
      thisObj.switchEditing($taskData)
    })
  }

  Sparkle.prototype.switchEditing = function ($taskData) {
    this._transition('editing')

    // Plan can't reload while user is editing a task
    this.cancelReload()

    // Make a box thingy
    this.cursor = new TaskEditor($taskData)

    // When user clicks outside task, save changes
    var thisObj = this
    var $taskTitle = $taskData.find('.task-title')
    this.u.onoff($taskTitle, 'blur', function () {
      thisObj.save()
    }).onoff($taskTitle, 'keydown', function (e) {
      if (e.which === 13 && !e.shiftKey) {
        // <Return>
        thisObj.save()
      }
    })
  }

  Sparkle.prototype.save = function () {
    console.assert(this.cursor, 'The task being edited cannot be null')

    // The user shouldn't be able to make changes while it's saving
    this.switchLocked()

    // Send the request
    var thisObj = this
    return this.cursor.save().then(function () {
      return thisObj.reload()
    }).fail(function () {
      thisObj.connectionLost()
    })
  }

  Sparkle.prototype.switchLocked = function ($taskData) {
    this._transition('locked')
  }

  // Schedule a reload of the plan.
  Sparkle.prototype.reload = function () {
    console.log('Reloading plan')

    this.cancelReload()

    var thisObj = this
    this._reloadDeferred = $.get('/', {plain: true}, function (data) {
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

  function TaskEditor($taskData) {
    this.id = $taskData.closest('.task').data('id')
    console.log('Editing task <%s>', this.id)
    this.$taskData = $taskData
    $taskData.find('.task-title').attr('contenteditable', 'true')
  }

  TaskEditor.prototype.save = function () {
    console.log('Saving task <%s>', this.id)

    var done_ = this.$taskData.find('.task-done :checkbox').prop('checked')
    var $taskTitle = this.$taskData.find('.task-title')
    var title_ = $taskTitle.text()

    // Make task read-only again
    $taskTitle.removeAttr('contenteditable')

    // Send it awayways
    return $.post('/api/v0/tasks/'+this.id+'/data',
                  JSON.stringify({done: done_, title: title_}))
  }

  return Sparkle

})(jQuery)

jQuery(function () {
  var s = new Sparkle('#plan')
  s.reload()
})
