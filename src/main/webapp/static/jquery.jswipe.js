(function ($) {
	$.fn.swipe = function (options) {
		var defaults = {
			thresholdHorizontal: { x: 30, y: 10 }, 
			thresholdVertical: { x: 10, y: 30 }, 
			swipeLeft: function() { alert('swiped left') }, 
			swipeRight: function() { alert('swiped right') },
			swipeUp: function() { alert('swiped up') }, 
			swipeDown: function() { alert('swiped down') }
		};
		var options = $.extend(defaults, options);
		if (!this)
			return false;
		return this.each(function() {
			var me = $(this);
			var originalCoord = { x: 0, y: 0 };
			var finalCoord = { x: 0, y: 0 };
			function touchStart(event) {
				originalCoord.x = event.targetTouches[0].pageX;
				originalCoord.y = event.targetTouches[0].pageY;
			}
			function touchMove(event) {
				event.preventDefault();
				finalCoord.x = event.targetTouches[0].pageX;
				finalCoord.y = event.targetTouches[0].pageY;
			}
			function touchEnd(event) {
				var changeY = originalCoord.y - finalCoord.y;
				var changeX = originalCoord.x - finalCoord.x;
				if (changeY < defaults.thresholdHorizontal.y && changeY > (defaults.thresholdHorizontal.y * -1)) {
					if (changeX > defaults.thresholdHorizontal.x) {
						defaults.swipeLeft();
					}
					if (changeX < (defaults.thresholdHorizontal.x * -1)) {
						defaults.swipeRight();
					}
				}
				if (changeX < defaults.thresholdVertical.x && changeX > (defaults.thresholdVertical.x * -1)) {
					if (changeY > defaults.thresholdVertical.x) {
						defaults.swipeUp();
					}
					if (changeY < (defaults.thresholdVertical.x * -1)) {
						defaults.swipeDown();
					}
				}

			}
			function touchStart(event) {
				originalCoord.x = event.targetTouches[0].pageX;
				originalCoord.y = event.targetTouches[0].pageY;
				finalCoord.x = originalCoord.x;
				finalCoord.y = originalCoord.y;
			}
			function touchCancel(event) {
			}
			this.addEventListener("touchstart", touchStart, false);
			this.addEventListener("touchmove", touchMove, false);
			this.addEventListener("touchend", touchEnd, false);
			this.addEventListener("touchcancel", touchCancel, false);
		});
    };
})(jQuery);