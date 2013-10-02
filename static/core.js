'use strict'

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

jQuery.fn.extend({
    fuzzyCheckboxes: function (selector) {
        this.on('click', selector, function () {
            $(this).find(':checkbox').click()
            })
        this.on('click', selector + ' :checkbox', function (e) {
            e.stopPropagation()
            })
        }
    })

function Sparkle(root) {
    this.root = root
    root.fuzzyCheckboxes('.task-done')

    var state = Locked
    // Note: defineProperty only works in IE >= 9
    Object.defineProperty(this, 'state', {
        get: function() { return state },
        set: function(state_) {
            state = state_
            this.hookUp()
            }
        })
    this.hookUp()
    }

Sparkle.prototype.hookUp = function() {
    console.log('state is ' + this.state)

    var sparkleRoot = this.root
    states.forEach(function(s) { sparkleRoot.removeClass(s) })
    this.root.addClass(this.state)

    if (this.state != Locked)
        this.root.find('input').removeAttr('disabled')
    else
        this.root.find('input').attr('disabled', true)
    }

Sparkle.prototype.loop = function() {
    var thisObj = this
    $.get('/', {plain: true}, function(data) {
        // Only update the plan when the user isn't editing it
        if (thisObj.state !== Editing) {
            thisObj.root.html(data)
            thisObj.state = Idle
            setTimeout(function() { thisObj.loop() }, 5000)
            }
        })
    .fail(function() {
        thisObj.state = Locked
        alert('Connection lost. Reload the page and try again.')
        })
    }

$(function() {
    var s = new Sparkle($('#plan'))
    s.loop()
    })
