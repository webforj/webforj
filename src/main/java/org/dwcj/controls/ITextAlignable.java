package org.dwcj.controls;

public interface ITextAlignable {
    
    enum Alignment {
        LEFT(8192),
        MIDDLE(16384),
        RIGHT(32768);

        public final Integer textPosition;
        
        private Alignment(Integer position){
            this.textPosition = position;
        }
    }

    Alignment getTextAlignment();

    ITextAlignable setTextAlignment(Alignment alignment);

}
