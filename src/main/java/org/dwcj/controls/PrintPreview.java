package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjPrintPreview;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class PrintPreview extends AbstractDwcControl {

    private BBjPrintPreview printPreview;

    public PrintPreview() {}

    @Override
    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addPrintPreview(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", null);
            catchUp();
            printPreview = (BBjPrintPreview) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void firstPage() {
        printPreview.firstPage();
    }

    public void fitToHeight() {
        printPreview.fitToHeight();
    }

    public void fitToWidth() {
        printPreview.fitToWidth();
    }

    public int getIndex() {
        return printPreview.getIndex();
    }

    public int getPageCount() {
        return printPreview.getPageCount();
    }

    public double getZoom() {
        return printPreview.getZoom();
    }

    public void lastPage() {
        printPreview.lastPage();
    }

    public void nextPage() {
        printPreview.nextPage();
    }

    public void previousPage() {
        printPreview.previousPage();
    }

    public void scrollDown() {
        printPreview.scrollDown();
    }

    public void scrollUp() {
        printPreview.scrollUp();
    }

    public void setIndex(int pageNumber) {
        printPreview.setIndex(pageNumber);
    }

    public void setZoom(double zoomAmount) {
        printPreview.setZoom(zoomAmount);
    }

    public void twoPage() {
        printPreview.twoPage();
    }
}
