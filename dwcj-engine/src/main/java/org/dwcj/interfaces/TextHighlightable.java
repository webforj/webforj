package org.dwcj.interfaces;

/**
 * On applicable controls, creates enum which 
 * helps facilitate underlying BBj constant integers for text highlighting
 * behavior to legible enum values, and facilitates implementation of methods
 * to interact with this behavior.
 */
public interface TextHighlightable {
    
    enum Highlight {
        HIGHLIGHT_NONE(0),
        HIGHLIGHT_KEY(1),
        HIGHLIGHT_MOUSE(2),
        HIGHLIGHT_KEY_MOUSE(3),
        HIGHLIGHT_FOCUS(4),
        HIGHLIGHT_FOCUS_OR_KEY(5),
        HIGHLIGHT_FOCUS_OR_MOUSE(6),
        HIGHLIGHT_ALL(7);

        public final Integer highlightType;
        
        private Highlight(Integer num){
            this.highlightType = num;
        }
    }

    /**
     * This method returns an enum representing the current behavior of highlighting in a text control when the control receives focus.
     * @return Enum representing BBj constant for highlight on focus
     */
    Highlight getHighlightOnFocus();

    /**
     * This method sets the behavior of controls implementing the TextControl interface when receiving focus via the keyboard, mouse, or BBjControl::focus() method FOCUS mnemonic.
     * @param highlight Enum representing a BBj constant that dictates highlight behavior for control
     * @return The control itself.
     */
    TextHighlightable setHighlightOnFocus(Highlight highlight);
}
