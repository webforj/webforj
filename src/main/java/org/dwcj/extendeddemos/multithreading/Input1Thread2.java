package org.dwcj.extendeddemos.multithreading;

import org.dwcj.controls.textbox.TextBox;
import org.dwcj.controls.button.events.ButtonPushEvent;

public class Input1Thread2 implements Runnable {
    private TextBox input1;
    private TextBox input2;
    private boolean doReset = false;
    private static final boolean ALWAYSCONTINUE = true;


    @Override
    public void run() {
        while (ALWAYSCONTINUE) {
            try {
                Thread.sleep(4000);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }

            input1.setText("1");

            int x=0;
            if (this.doReset)
                this.doReset = false;
            else
                x = Integer.valueOf(input2.getText());
            x=x+1;
            input2.setText(String.valueOf(x));

        }

    }

    public void setInput1(TextBox input1) {
        this.input1 = input1;
    }

    public void setInput2(TextBox input2) {
        this.input2 = input2;
    }

    /**
     * executed directly from the UI thread
     * @param buttonPushEvent - the event object received from the UI
     */
    public void onReset(ButtonPushEvent buttonPushEvent) { // no problem - is consumer for events
        this.doReset = true;
    }
}
