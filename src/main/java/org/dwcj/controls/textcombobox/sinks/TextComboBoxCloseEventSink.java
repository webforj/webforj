package org.dwcj.controls.textcombobox.sinks;

import com.basis.bbj.proxies.event.BBjListCloseEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.textcombobox.TextComboBox;
import org.dwcj.controls.textcombobox.events.TextComboBoxCloseEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class TextComboBoxCloseEventSink {
    
    private ArrayList<Consumer<TextComboBoxCloseEvent>> targets = new ArrayList<>();
    private final TextComboBox textComboBox;
    private BBjControl bbjctrl;

    @SuppressWarnings({"static-access"})
    public TextComboBoxCloseEventSink(TextComboBox cb) {
        this.textComboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CLOSE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "closeEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings({"static-access"})
    public TextComboBoxCloseEventSink(TextComboBox cb, Consumer<TextComboBoxCloseEvent> callback) {
        this.targets.add(callback);
        this.textComboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CLOSE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "closeEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings("java.S1172")
    public void closeEvent(BBjListCloseEvent ev) { //NOSONAR
        TextComboBoxCloseEvent dwcEv = new TextComboBoxCloseEvent(this.textComboBox);
        Iterator<Consumer<TextComboBoxCloseEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<TextComboBoxCloseEvent> callback) { targets.add(callback); }



}
