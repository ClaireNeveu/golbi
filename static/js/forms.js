/// <reference path="jquery.d.ts" />
var Forms = (function ($) {
    return {
        handler: function (method) {
            return function (e) {
                e.preventDefault();
                var foo = $.ajax({
                    url: $(this).attr('action'),
                    type: method,
                    data: $(this).serialize(),
                    async: false,
                    success: function (data, textStatus, jq) {
                        var redirect = jq.getResponseHeader('Golbi-Redirect');
                        if (redirect) {
                            window.location.href = redirect;
                        }
                    }
                });

                //$(':root').html(foo.responseText);
                return false;
            };
        },
        rig: function (element) {
            return element.bind('submit', this.handler(element.attr('method')));
        }
    };
}($));

Forms.rig($('.jsForm'));
