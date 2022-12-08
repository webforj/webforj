package org.dwcj.extendeddemos.multithreading;

import org.dwcj.App;
import org.dwcj.controls.button.Button;
import org.dwcj.controls.label.Label;
import org.dwcj.controls.textbox.TextBox;
import org.dwcj.controls.button.events.ButtonPushEvent;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.controls.panels.AppPanel;

public class MultiThreadingDemo extends App {

    Thread thread1;
    Thread thread2;

    @Override
    public void run() throws DwcAppInitializeException {
        AppPanel panel = new AppPanel();
        panel.setStyle("display","inline-grid");
        panel.setStyle("grid-display-columns","1fr 1fr");
        panel.setStyle("gap","20px");
        panel.setStyle("padding","20px");

        Label headline = new Label("<html><h1>MultiThreading Demo</h1>");
        headline.setStyle("grid-column","1 / span 2");
        panel.add(headline);

        Label explanation = new Label("<html><p>You see two threads. The first one increases field 1 every 1/100s, the second one the second field every fou seconds. The event handlers invoke methods directly in the respective child threads.</p>");
        explanation.setStyle("grid-column","1 / span 2");
        explanation.setStyle("width","500px");
        panel.add(explanation);

        TextBox input1 = new TextBox("1");
        panel.add(input1);
        Button reset1 = new Button("Reset");
        panel.add(reset1);

        TextBox input2 = new TextBox("1");
        panel.add(input2);
        Button reset2 = new Button("Reset");
        panel.add(reset2);


        Button b = new Button("Stop");
        panel.add(b);
        b.onClick(this::close);


        Input1Thread1 t1 = new Input1Thread1();
        reset1.onClick(t1::onReset);

        t1.setInput1(input1);
        thread1 = new Thread(t1);

        Input1Thread2 t2 = new Input1Thread2();
        reset2.onClick(t2::onReset);
        t2.setInput1(input1);
        t2.setInput2(input2);
        thread2 = new Thread(t2);

        thread1.start();
        thread2.start();

    }

    @Override
    public void cleanup() {
        super.cleanup();
        thread1.interrupt();
        thread2.interrupt();
    }

    private void close(ButtonPushEvent buttonPushEvent) {
        terminate();
    }
}
