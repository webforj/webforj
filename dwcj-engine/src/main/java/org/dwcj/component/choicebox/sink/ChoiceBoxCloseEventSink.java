package org.dwcj.component.choicebox.sink;

import com.basis.bbj.proxies.event.BBjListCloseEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.choicebox.ComboBox;
import org.dwcj.component.choicebox.event.ChoiceBoxCloseEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;


public class ChoiceBoxCloseEventSink {
    
    private ArrayList<Consumer<ChoiceBoxCloseEvent>> targets = new ArrayList<>();
    private final ComboBox comboBox;
    private BBjControl bbjctrl;

    @SuppressWarnings({"static-access"})
    public ChoiceBoxCloseEventSink(ComboBox cb) {
        this.comboBox = cb;

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
    public ChoiceBoxCloseEventSink(ComboBox cb, Consumer<ChoiceBoxCloseEvent> callback) {
        this.targets.add(callback);
        this.comboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CLOSE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "closeEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java.S1172")
    public void closeEvent(BBjListCloseEvent ev) { //NOSONAR
        ChoiceBoxCloseEvent dwcEv = new ChoiceBoxCloseEvent(this.comboBox);
        Iterator<Consumer<ChoiceBoxCloseEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ChoiceBoxCloseEvent> callback) { targets.add(callback); }

}
