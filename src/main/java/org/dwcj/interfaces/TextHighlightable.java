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

        public final Integer highlight;
        
        private Highlight(Integer num){
            this.highlight = num;
        }
    }

    Highlight getHighlightOnFocus();

    TextHighlightable setHighlightOnFocus(Highlight highlight);
}
