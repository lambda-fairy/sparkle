'use strict'

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

    var editable = false
    // Note: defineProperty only works in IE >= 9
    Object.defineProperty(this, 'editable', {
        get: function() { return editable },
        set: function(yesno) {
            editable = yesno
            this.hookUp()
            }
        })

    }

Sparkle.prototype.hookUp = function() {
    this.root
        .toggleClass('editable', this.editable)
        .toggleClass('not-editable', !this.editable)
    if (this.editable)
        this.root.find('input').removeAttr('disabled')
    else
        this.root.find('input').attr('disabled', true)
    }

Sparkle.prototype.loop = function() {
    var thisObj = this
    $.get('/', {plain: true}, function(data) {
        thisObj.root.html(data)
        thisObj.hookUp()
        setTimeout(function() { thisObj.loop() }, 5000)
        })
    }

$(function() {
    var s = new Sparkle($('#plan'))
    s.editable = true
    s.loop()
    })
