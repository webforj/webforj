package org.dwcj.extendeddemos.multithreading;

import org.dwcj.controls.textbox.TextBox;
import org.dwcj.controls.button.events.ButtonPushEvent;

public class Input1Thread1 implements Runnable {
    private TextBox input1;
    private boolean doReset = false;
    private static final boolean ALWAYSCONTINUE = true;


    @Override
    public void run() {
        while (ALWAYSCONTINUE) {
            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            int x=0;
            if (this.doReset)
                this.doReset = false;
            else
                x = Integer.valueOf(input1.getText());
            x=x+1;
            input1.setText(String.valueOf(x));
        }

    }

    public void setInput1(TextBox input1) {
        this.input1 = input1;
    }

    /**
     * executed directly from the UI thread
     * @param buttonPushEvent - the event object received from the UI
     */
    public void onReset(ButtonPushEvent buttonPushEvent) { // no issue, implements consumer method with predetermined signature
        this.doReset = true;
    }
}
