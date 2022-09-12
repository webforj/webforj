package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjNavigator;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.resource.RecordSet;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.navigator.NavigatorFirstEvent;
import org.dwcj.events.navigator.NavigatorLastEvent;
import org.dwcj.events.navigator.NavigatorNextEvent;
import org.dwcj.events.navigator.NavigatorPreviousEvent;
import org.dwcj.events.sinks.navigator.NavFirstEventSink;
import org.dwcj.events.sinks.navigator.NavLastEventSink;
import org.dwcj.events.sinks.navigator.NavNextEventSink;
import org.dwcj.events.sinks.navigator.NavPreviousEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.function.Consumer;

public final class Navigator extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    private BBjNavigator bbjNavigator;

    private NavFirstEventSink navFirstEventSink;
    private NavLastEventSink navLastEventSink;
    private NavNextEventSink navNextEventSink;
    private NavPreviousEventSink navPreviousEventSink;

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addNavigator(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            catchUp();
            bbjNavigator = (BBjNavigator) ctrl;
        } catch (Exception e)  {
            e.printStackTrace();
        }
    }

    public RecordSet getTargetRecordSet() {
        try {
            return (RecordSet) bbjNavigator.getTargetRecordSet();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public boolean isEditable() {
        try {
            return bbjNavigator.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public void setEditable(boolean editable) {
        try {
            bbjNavigator.setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public Navigator onFirst(Consumer<NavigatorFirstEvent> callback) {
        if (this.navFirstEventSink==null)
            this.navFirstEventSink = new NavFirstEventSink(this, callback);
        else this.navFirstEventSink.addCallback(callback);
        return this;
    }

    public Navigator onLast(Consumer<NavigatorLastEvent> callback) {
        if (this.navLastEventSink==null)
            this.navLastEventSink = new NavLastEventSink(this, callback);
        else this.navLastEventSink.addCallback(callback);
        return this;
    }

    public Navigator onNext(Consumer<NavigatorNextEvent> callback) {
        if (this.navNextEventSink==null)
            this.navNextEventSink = new NavNextEventSink(this, callback);
        else this.navNextEventSink.addCallback(callback);
        return this;
    }

    public Navigator onPrevious(Consumer<NavigatorPreviousEvent> callback) {
        if (this.navPreviousEventSink==null)
            this.navPreviousEventSink = new NavPreviousEventSink(this, callback);
        else this.navPreviousEventSink.addCallback(callback);
        return this;
    }

    @Override
    public Navigator setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public Navigator setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public Navigator addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public Navigator removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public Navigator setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
}
