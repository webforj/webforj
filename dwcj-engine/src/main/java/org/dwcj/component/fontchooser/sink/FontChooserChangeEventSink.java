package org.dwcj.component.fontchooser.sink;

import com.basis.bbj.proxies.event.BBjFileChooserChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.fontchooser.FontChooser;
import org.dwcj.component.fontchooser.event.FontChooserChangeEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class FontChooserChangeEventSink {

    private ArrayList<Consumer<FontChooserChangeEvent>> targets;

    private final FontChooser fontChooser;

    @SuppressWarnings({"static-access"})
    public FontChooserChangeEventSink(FontChooser fc, Consumer<FontChooserChangeEvent> callback) {
        this.targets.add(callback);
        this.fontChooser = fc;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(fc);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_FILECHOOSER_CHANGE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "changeEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
        
    }

    public void changeEvent(BBjFileChooserChangeEvent ev) { //NOSONAR
        FontChooserChangeEvent dwcEv = new FontChooserChangeEvent(this.fontChooser);
        Iterator<Consumer<FontChooserChangeEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<FontChooserChangeEvent> callback) { targets.add(callback); }
}
