package org.dwcj.interfaces;

public interface ITextHighlightable {
    
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

    ITextHighlightable setHighlightOnFocus(Highlight highlight);
}
