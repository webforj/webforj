package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjPrintPreview;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class PrintPreview extends AbstractDwcControl {

    private BBjPrintPreview bbjPrintPreview;


    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addPrintPreview(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", null);
            catchUp();
            bbjPrintPreview = (BBjPrintPreview) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void firstPage() {
        bbjPrintPreview.firstPage();
    }

    public void fitToHeight() {
        bbjPrintPreview.fitToHeight();
    }

    public void fitToWidth() {
        bbjPrintPreview.fitToWidth();
    }

    public int getIndex() {
        return bbjPrintPreview.getIndex();
    }

    public int getPageCount() {
        return bbjPrintPreview.getPageCount();
    }

    public double getZoom() {
        return bbjPrintPreview.getZoom();
    }

    public void lastPage() {
        bbjPrintPreview.lastPage();
    }

    public void nextPage() {
        bbjPrintPreview.nextPage();
    }

    public void previousPage() {
        bbjPrintPreview.previousPage();
    }

    public void scrollDown() {
        bbjPrintPreview.scrollDown();
    }

    public void scrollUp() {
        bbjPrintPreview.scrollUp();
    }

    public void setIndex(int pageNumber) {
        bbjPrintPreview.setIndex(pageNumber);
    }

    public void setZoom(double zoomAmount) {
        bbjPrintPreview.setZoom(zoomAmount);
    }

    public void twoPage() {
        bbjPrintPreview.twoPage();
    }

    @Override
    public PrintPreview setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public PrintPreview addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public PrintPreview removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }
}
