package org.dwcj.controls.textComboBox.sinks;

import com.basis.bbj.proxies.event.BBjListSelectEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.textComboBox.TextComboBox;
import org.dwcj.controls.textComboBox.events.TextComboBoxSelectEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class TextComboBoxSelectEventSink {

    private ArrayList<Consumer<TextComboBoxSelectEvent>> targets = new ArrayList<>();

    private final TextComboBox textComboBox;

    private BBjControl bbjctrl;

    @SuppressWarnings({"static-access"})
    public TextComboBoxSelectEventSink(TextComboBox cb) {
        this.textComboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_SELECT,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings({"static-access"})
    public TextComboBoxSelectEventSink(TextComboBox cb, Consumer<TextComboBoxSelectEvent> callback) {
        this.targets.add(callback);
        this.textComboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_SELECT,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings("java.S1172")
    public void selectEvent(BBjListSelectEvent ev) { //NOSONAR
        TextComboBoxSelectEvent dwcEv = new TextComboBoxSelectEvent(this.textComboBox);
        Iterator<Consumer<TextComboBoxSelectEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void doSelect(Object key) {
        TextComboBoxSelectEvent dwcEv = new TextComboBoxSelectEvent(this.textComboBox);
        dwcEv.setKey(key);
        Iterator<Consumer<TextComboBoxSelectEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<TextComboBoxSelectEvent> callback) { targets.add(callback); }
}
