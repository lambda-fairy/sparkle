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

  // The interface is a state machine that cycles through three states:
  // Locked, Idle, Editing.

  var Locked = 'locked'
  // The plan is locked and cannot be edited.  This can happen when:
  //   1. Sparkle is currently saving a modification
  //   2. Client cannot connect to the server

  var Idle = 'idle'
  // The default state.

  var Editing = 'editing'
  // The user has made a modification to a task, but has not saved it.
  // Client code must not update the plan in this state.

  var states = [Locked, Idle, Editing]

  function Sparkle(rootSelector) {
    this.$root = $(rootSelector)
    this.$root.fuzzyCheckboxes('.task-done')

    // Wire up event handlers
    this.cursor = null
    var thisObj = this
    this.$root
      .on('focus', '.task-title', function () {
        if (thisObj.state !== Idle)
          return

        var $taskData = $(this).closest('.task-data')
        thisObj.cursor = new TaskEditor($taskData)
        thisObj.state = Editing
      })
      .on('blur', '.task-title', function () {
        if (thisObj.state !== Editing)
          return
        console.assert(thisObj.cursor, 'The task being edited cannot be null')

        thisObj.state = Locked
        thisObj.cursor.finalize()
          .done(function () { thisObj.reload(); delete thisObj.cursor })
          .fail(function () { connectionLost() })
      })

    // Set initial state
    var state = Locked
    // Note: defineProperty only works in IE >= 9
    Object.defineProperty(this, 'state', {
      get: function () { return state },
      set: function (state_) {
        if (state_ !== state) {
          console.log('Switching state to: %s', state_)
          state = state_
        }
        this.applyState()
      }
    })
    this.applyState()
  }

  // Ensure the interface matches up with the internal state.  This is
  // called whenever the state changes, and after the plan successfully
  // reloads.
  Sparkle.prototype.applyState = function () {
    // Set CSS classes for styling
    var sparkleRoot = this.$root
    states.forEach(function (s) { sparkleRoot.removeClass(s) })
    this.$root.addClass(this.state)

    // Enable/disable all input fields
    if (this.state !== Locked)
      this.$root.find('input').removeAttr('disabled')
    else
      this.$root.find('input').attr('disabled', true)

    // Plan can't reload while user is editing a task
    if (this.state === Editing)
      this.cancelReload()
  }

  // Schedule a reload of the plan.
  Sparkle.prototype.reload = function () {
    console.log('Reloading plan')

    this.cancelReload()

    var thisObj = this
    this._reloadDeferred = $.get('/', {plain: true}, function (data) {
      console.assert(
        thisObj.state !== Editing,
        'The plan should never reload while the user is editing it')
      thisObj.$root.html(data)
      thisObj.state = Idle
      thisObj._reloadTimer = setTimeout(function () { thisObj.reload() }, 5000)
    }).fail(function (ajax) {
      if (ajax.statusText !== 'abort') {
        // This failed because of an actual error, not cancelReload()
        thisObj.state = Locked
        connectionLost()
      }
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

  function TaskEditor($taskData) {
    this.id = $taskData.closest('.task').data('id')
    console.log('Editing task <%s>', this.id)
    this.$taskData = $taskData
    $taskData.find('.task-title').attr('contenteditable', 'true')
  }

  TaskEditor.prototype.finalize = function () {
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

  function connectionLost() {
    alert('Connection lost. Reload the page and try again.')
  }

  return Sparkle

})(jQuery)

jQuery(function () {
  var s = new Sparkle('#plan')
  s.reload()
})
