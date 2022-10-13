package org.dwcj.controls;

public interface IMouseWheelEnableable {
    
    enum MouseWheelCondition{
        DEFAULT(0),
        NEVER(1),
        FOCUS(2),
        MOUSE_OVER(3),
        FOCUS_AND_MOUSE_OVER(4),
        MOUSE_THEN_FOCUS(5),
        FOCUS_THEN_MOUSE(6);

        public final Integer mouseWheelEnabledCondition;

        private MouseWheelCondition(Integer condition){
            this.mouseWheelEnabledCondition = condition;
        }
    }

    MouseWheelCondition getScrollWheelBehavior();

    IMouseWheelEnableable setScrollWheelBehavior(MouseWheelCondition condition);


}
