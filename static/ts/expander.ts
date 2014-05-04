/// <reference path="jquery.d.ts" />

var Expander = (function ($: JQueryStatic) {
    return {
        handler: function (element: JQuery) {
            return function (e: Event) {
                e.preventDefault();
                if (element.hasClass('collapsed')) {
                    var height: number = 0;
                    element.children().each(function(){
                        height = height + $(this).outerHeight();
                    });
                    element.css({ "max-height": height + 'px' });
                } else {
                    element.css({ "max-height": "0px" });
                }
                element.toggleClass('collapsed');
                return false;
            }
        },
        rig: function (element: JQuery) {
            return element.find('.expandButton')
                .bind('click', this.handler(element.find('.expandArea')));
        }
    }
}($));

Expander.rig($('.formWrapper'));