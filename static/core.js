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
    }

Sparkle.prototype.setEditable = function (yesno) {
    this.root.toggleClass('editable', yesno).toggleClass('not-editable', !yesno)
    if (yesno)
        this.root.find('input').removeAttr('disabled')
    else
        this.root.find('input').attr('disabled', true)
    }

$(function () {
    var s = new Sparkle($('#plan'))
    s.setEditable(true)
    })
