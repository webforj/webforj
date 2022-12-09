package org.dwcj.controls.printpreview;

import com.basis.bbj.proxies.sysgui.BBjPrintPreview;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;

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



    public PrintPreview setText(String text) {
        super.setControlText(text);
        return this;
    }

    public PrintPreview setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public PrintPreview setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public PrintPreview setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public PrintPreview setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public PrintPreview setID(String id){
        super.setControlID(id);
        return this;
    }

    public PrintPreview setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public PrintPreview addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public PrintPreview removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }


}
