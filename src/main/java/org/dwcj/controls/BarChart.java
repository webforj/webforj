package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjBarChart;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class BarChart extends AbstractDwcControl implements IControl, IStyleable {

    private BBjBarChart bbjBarChart;

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            ctrl = w.addBarChart(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "x","y",0,0,false,false,false);
            catchUp();
            bbjBarChart = (BBjBarChart) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public boolean is3D() {
        return bbjBarChart.is3D();
    }

    public int getCategoryCount() {
        return bbjBarChart.getCategoryCount();
    }

    public int getSeriesCount() {
        return bbjBarChart.getSeriesCount();
    }

    public String getTitle() {
        return bbjBarChart.getTitle();
    }

    public String getXLabel() {
        return bbjBarChart.getXLabel();
    }

    public String getYLabel() {
        return bbjBarChart.getYLabel();
    }

    public boolean isLegendShown() {
        return bbjBarChart.isLegendShown();
    }

    public BarChart setBarValue(int series, int category, int value) {
        try {
            bbjBarChart.setBarValue(series, category, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public BarChart setCategoryCount(int categories) {
        bbjBarChart.setCategoryCount(categories);
        return this;
    }

    public BarChart setCategoryName(int category, String name) {
        try {
            bbjBarChart.setCategoryName(category, name);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public BarChart setLegendShown(boolean legendShown) {
        bbjBarChart.setLegendShown(legendShown);
        return this;
    }

    public BarChart setSeriesCount(int series) {
        bbjBarChart.setSeriesCount(series);
        return this;
    }

    public BarChart setSeriesName(int series, String name) {
        try {
            bbjBarChart.setSeriesName(series, name);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public BarChart setTitle(String title) {
        bbjBarChart.setTitle(title);
        return this;
    }

    public BarChart setXLabel(String label) {
        bbjBarChart.setXLabel(label); //Fixed copy/paste error here - MH
        return this;
    }

    public BarChart setYLabel(String label) {
        bbjBarChart.setYLabel(label);
        return this;
    }

    @Override
    public IStyleable setStyle(String property, String value) {
        super.setControlStyle(property,value);
        return this;
    }

    @Override
    public IStyleable addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public IStyleable removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }
}
