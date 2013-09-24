function Sparkle(root) {
    this.root = root
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
