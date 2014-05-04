/// <reference path="jquery.d.ts" />
var Expander = (function ($) {
    return {
        handler: function (element) {
            return function (e) {
                e.preventDefault();
                if (element.hasClass('collapsed')) {
                    var height = 0;
                    element.children().each(function () {
                        height = height + $(this).outerHeight();
                    });
                    element.css({ "max-height": height + 'px' });
                } else {
                    element.css({ "max-height": "0px" });
                }
                element.toggleClass('collapsed');
                return false;
            };
        },
        rig: function (element) {
            return element.find('.expandButton').bind('click', this.handler(element.find('.expandArea')));
        }
    };
}($));

Expander.rig($('.formWrapper'));
