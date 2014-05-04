/// <reference path="jquery.d.ts" />

var Forms = (function ($: JQueryStatic) {
    return {
        handler: function (method: string) {
            return function (e: Event) {
                e.preventDefault();
                var foo = $.ajax({
                    url: $(this).attr('action'),
                    type: method,
                    data: $(this).serialize(),
                    async: false,
                    success: function(data: Object, textStatus: string, jq: JQueryXHR) {
                        var redirect = jq.getResponseHeader('Golbi-Redirect');
                        if (redirect) {
                            window.location.href = redirect;
                        }
                    }
                });
                //$(':root').html(foo.responseText);
                return false;
            }
        },
        rig: function (element: JQuery) {
            return element.bind('submit', this.handler(element.attr('method')));
        }
    }
}($));

Forms.rig($('.jsForm'));