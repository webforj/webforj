package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjBarChart;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class BarChart extends AbstractDwcControl implements IControl, IStyleable {

    private BBjBarChart barChart;

    public BarChart() {}

    @Override
    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            ctrl = w.addBarChart(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "x","y",0,0,false,false,false);
            catchUp();
            barChart = (BBjBarChart) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public boolean is3D() {
        return barChart.is3D();
    }

    public int getCategoryCount() {
        return barChart.getCategoryCount();
    }

    public int getSeriesCount() {
        return barChart.getSeriesCount();
    }

    public String getTitle() {
        return barChart.getTitle();
    }

    public String getXLabel() {
        return barChart.getXLabel();
    }

    public String getYLabel() {
        return barChart.getYLabel();
    }

    public boolean isLegendShown() {
        return barChart.isLegendShown();
    }

    public void setBarValue(int series, int category, int value) {
        try {
            barChart.setBarValue(series, category, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setCategoryCount(int categories) {
        barChart.setCategoryCount(categories);
    }

    public void setCategoryName(int category, String name) {
        try {
            barChart.setCategoryName(category, name);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setLegendShown(boolean legendShown) {
        barChart.setLegendShown(legendShown);
    }

    public void setSeriesCount(int series) {
        barChart.setSeriesCount(series);
    }

    public void setSeriesName(int series, String name) {
        try {
            barChart.setSeriesName(series, name);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setTitle(String title) {
        barChart.setTitle(title);
    }

    public void setXLabel(String label) {
        barChart.setTitle(label);
    }

    public void setYLabel(String label) {
        barChart.setYLabel(label);
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
