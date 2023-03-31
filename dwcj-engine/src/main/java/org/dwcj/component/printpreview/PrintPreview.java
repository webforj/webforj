package org.dwcj.component.printpreview;

import com.basis.bbj.proxies.sysgui.BBjPrintPreview;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.component.AbstractDwcControl;
import org.dwcj.component.panels.AbstractPanel;

public final class PrintPreview extends AbstractDwcControl {

    private BBjPrintPreview bbjPrintPreview;


    @Override
    protected void create(AbstractPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addPrintPreview(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", null);
            catchUp();
            bbjPrintPreview = (BBjPrintPreview) ctrl;
        } catch (Exception e) {
            Environment.logError(e);
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
    public PrintPreview setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public PrintPreview setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public PrintPreview setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public PrintPreview setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public PrintPreview setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public PrintPreview setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public PrintPreview setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public PrintPreview addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public PrintPreview removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }


}
