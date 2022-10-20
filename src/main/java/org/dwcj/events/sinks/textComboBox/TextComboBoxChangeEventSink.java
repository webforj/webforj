package org.dwcj.events.sinks.textComboBox;

import com.basis.bbj.proxies.event.BBjListChangeEvent;
import com.basis.bbj.proxies.event.BBjListSelectEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.TextComboBox;
import org.dwcj.events.textComboBox.TextComboBoxChangeEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;



public class TextComboBoxChangeEventSink {

    private ArrayList<Consumer<TextComboBoxChangeEvent>> targets = new ArrayList<>();

    private final TextComboBox textComboBox;

    private BBjControl bbjctrl;

    @SuppressWarnings({"static-access"})
    public TextComboBoxChangeEventSink(TextComboBox cb, Consumer<TextComboBoxChangeEvent> callback) {
        this.targets.add(callback);
        this.textComboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CHANGE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "changeEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings("java.S1172")
    public void changeEvent(BBjListChangeEvent ev) { //NOSONAR
        TextComboBoxChangeEvent dwcEv = new TextComboBoxChangeEvent(this.textComboBox);
        Iterator<Consumer<TextComboBoxChangeEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<TextComboBoxChangeEvent> callback) { targets.add(callback); }
    
}
