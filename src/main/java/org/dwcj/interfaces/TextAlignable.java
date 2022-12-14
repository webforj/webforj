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

    /**
     * Returns a value indicating the text's horizontal alignment.
     * @return
     */
    Alignment getTextAlignment();

    /**
     * Sets the horizontal alignment of the text within the control.
     * @param alignment Enum from list representing an internal BBj numeric constant
     * @return The control itself
     */
    TextAlignable setTextAlignment(Alignment alignment);

}
