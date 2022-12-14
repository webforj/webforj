package org.dwcj.interfaces;


/**
 * On applicable controls, creates enum which 
 * helps facilitate underlying BBj constant integers for text alignment 
 * behavior to legible enum values, and facilitates implementation of methods
 * to interact with this behavior.
 */
public interface TextAlignable {
    
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

    TextAlignable setTextAlignment(Alignment alignment);

}
