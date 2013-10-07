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
  var handlers = {
    // Placeholder thingie.
    undefined: {
      off: function () {}
    },

    // The plan is locked and cannot be edited.  This can happen when:
    //   1. Sparkle is currently saving a modification
    //   2. Client cannot connect to the server
    locked: {
      on: function () {
        this.setEnabled(false)
      },
      off: function () {
        this.setEnabled(true)
      }
    },

    // The default state.
    idle: {
      on: function () {
        var thisObj = this
        this.$root.on('focus.sparkle-idle', '.task-title', function () {
          var $taskData = $(this).closest('.task-data')
          thisObj.cursor = new TaskEditor($taskData)
          thisObj.transition('editing')
        })
      },
      off: function () {
        this.$root.off('.sparkle-idle')
      }
    },

    // The user has made a modification to a task, but has not saved it.
    // Client code must not update the plan in this state.
    editing: {
      on: function () {
        // Plan can't reload while user is editing a task
        this.cancelReload()

        // When user clicks outside task, save changes
        var thisObj = this
        this.$root.on('blur.sparkle-editing', '.task-title', function () {
          console.assert(thisObj.cursor, 'The task being edited cannot be null')
          thisObj.transition('locked')
        })
      },
      off: function () {
        this.$root.off('.sparkle-editing')
        var thisObj = this
        this.cursor.finalize()
          .done(function () { thisObj.reload(); delete thisObj.cursor })
          .fail(function () { connectionLost() })
      }
    }
  }

  function Sparkle(rootSelector) {
    this.$root = $(rootSelector)
    this.$root.fuzzyCheckboxes('.task-done')

    // Set initial state
    this.transition('locked')
  }

  Sparkle.prototype.setEnabled = function (hooray) {
    this.$root.find('input').prop('disabled', !hooray)
  }

  // Transition from one state to another.
  Sparkle.prototype.transition = function (newState) {
    var oldState = this.state
    if (newState !== oldState) {
      console.log('Transitioning from %s -> %s', oldState, newState)
      this.state = newState
    }

    // Set CSS classes for styling
    for (var s in handlers) if (handlers.hasOwnProperty(s)) {
      this.$root.removeClass(s)
    }
    this.$root.addClass(newState)

    // Invoke callbacks
    handlers[oldState].off.call(this)
    handlers[newState].on.call(this)
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
      // The given HTML has everything disabled by default, so we need
      // to re-enable it
      thisObj.setEnabled(true)
      thisObj.transition('idle')
      thisObj._reloadTimer = setTimeout(function () { thisObj.reload() }, 5000)
    }).fail(function (ajax) {
      if (ajax.statusText !== 'abort') {
        // This failed because of an actual error, not cancelReload()
        thisObj.connectionLost()
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

  Sparkle.prototype.connectionLost = function () {
    this.transition('locked')
    alert('Connection lost. Reload the page and try again.')
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

  return Sparkle

})(jQuery)

jQuery(function () {
  var s = new Sparkle('#plan')
  s.reload()
})
