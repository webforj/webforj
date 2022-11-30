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

public final class Navigator extends AbstractDwcControl implements IReadOnly{

    private BBjNavigator bbjNavigator;

    private NavFirstEventSink navFirstEventSink;
    private NavLastEventSink navLastEventSink;
    private NavNextEventSink navNextEventSink;
    private NavPreviousEventSink navPreviousEventSink;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }

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

    @Override
    public Boolean isReadOnly() {
        try {
            return bbjNavigator.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public Navigator setReadOnly(Boolean editable) {
        try {
            bbjNavigator.setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
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




    public Navigator setText(String text) {
        super.setControlText(text);
        return this;
    }

    public Navigator setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public Navigator setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public Navigator setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public Navigator setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public Navigator setID(String id){
        super.setControlID(id);
        return this;
    }

    public Navigator setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public Navigator addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public Navigator removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }




    public Navigator setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public Navigator setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

}
