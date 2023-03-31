package org.dwcj.component.choicebox.sink;

import com.basis.bbj.proxies.event.BBjListSelectEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.choicebox.ComboBox;
import org.dwcj.component.choicebox.event.ChoiceBoxSelectEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class ComboBoxSelectEventSink {

    private ArrayList<Consumer<ChoiceBoxSelectEvent>> targets = new ArrayList<>();

    private final ComboBox comboBox;

    private BBjControl bbjctrl;

    @SuppressWarnings({"static-access"})
    public ComboBoxSelectEventSink(ComboBox cb) {
        this.comboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_SELECT,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings({"static-access"})
    public ComboBoxSelectEventSink(ComboBox cb, Consumer<ChoiceBoxSelectEvent> callback) {
        this.targets.add(callback);
        this.comboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_SELECT,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java.S1172")
    public void selectEvent(BBjListSelectEvent ev) { //NOSONAR
        ChoiceBoxSelectEvent dwcEv = new ChoiceBoxSelectEvent(this.comboBox);
        Iterator<Consumer<ChoiceBoxSelectEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void doSelect(Object key) {
        ChoiceBoxSelectEvent dwcEv = new ChoiceBoxSelectEvent(comboBox);
        dwcEv.setKey(key);
        Iterator<Consumer<ChoiceBoxSelectEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ChoiceBoxSelectEvent> callback) { targets.add(callback); }
}
