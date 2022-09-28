package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjBarChart;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class BarChart extends AbstractDwcControl implements IControl {

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

    /**
     * Returns true or false based on whether or not the chart has been created as a 3D chart or not.
     * @return boolean
     */
    public boolean is3D() {
        return bbjBarChart.is3D();
    }

    /**
     * 
     * Returns the number of categories in the chart. Categories are groups of data.
     * @return int
     */
    public int getCategoryCount() {
        return bbjBarChart.getCategoryCount();
    }

    /**
     * 
     * Returns the number of series in the chart. Series are the pieces of data, or bars, within a category.
     * @return int
     */
    public int getSeriesCount() {
        return bbjBarChart.getSeriesCount();
    }

    /**
     * 
     * Returns the title of the chart.
     * @return String
     */
    public String getTitle() {
        return bbjBarChart.getTitle();
    }

    /**
     * 
     * Returns the X axis label of the chart.
     * @return String
     */
    public String getXLabel() {
        return bbjBarChart.getXLabel();
    }

    /**
     * 
     * Returns the Y axis label of the chart.
     * @return String
     */
    public String getYLabel() {
        return bbjBarChart.getYLabel();
    }

    /**
     * 
     * Returns a true or false value based on whether the legend for the chart is currently visable.
     * @return boolean
     */
    public boolean isLegendShown() {
        return bbjBarChart.isLegendShown();
    }

    /**
     * Sets the value of a single bar within the chart. Three integers must be given:
     * 1) The desired series
     * 2) The desired category
     * 3) The value you wish the data to reflect.
     * Function returns instance of the object to allow for method chaining.
     * Returns a true or false value based on whether the legend for the chart is currently visable.
     * @param series - The piece of data, or singular bar, you want to edit
     * @param category - The group of data you want to edit
     * @param value - the value you want to give this datapoint
     * @return BarChart
     */
    public BarChart setBarValue(int series, int category, int value) {
        try {
            bbjBarChart.setBarValue(series, category, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * 
     * Sets the number of discrete categories, or groups, of data your chart will have.
     * @param categoryNumber - How many groups of data you want the chart to have.
     * @return boolean
     */
    public BarChart setCategoryCount(int categories) {
        bbjBarChart.setCategoryCount(categories);
        return this;
    }

    /**
     * 
     * Sets the name of a group of data, or category, on the chart. Must specify category number first (zero based), then a name.
     * @param category - Selects which of the categories in your chart you wish to rename
     * @param name - The desired name of the category
     * @return BarChart
     */
    public BarChart setCategoryName(int category, String name) {
        try {
            bbjBarChart.setCategoryName(category, name);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * 
     * Sets whether or not there will be a legend for the bar chart shown.
     * @param legendShown - Boolean for whether or not the legend should be shown
     * @return BarChart
     */
    public BarChart setLegendShown(boolean legendShown) {
        bbjBarChart.setLegendShown(legendShown);
        return this;
    }

    /**
     * 
     * Sets the number of series, or pieces of data within a category, the chart will have.
     * @param series - Integer for how many series, or pieces of data you wish each of your categories to have.
     * @return BarChart
     */
    public BarChart setSeriesCount(int series) {
        bbjBarChart.setSeriesCount(series);
        return this;
    }

    /**
     * 
     * Sets the name of a group of data, or category, on the chart. Must specify category number first (zero based), then a name.
     * @param series - Integer which selects the series you wish to modify
     * @param name - String for the name you want to give your series
     * @return BarChart
     */
    public BarChart setSeriesName(int series, String name) {
        try {
            bbjBarChart.setSeriesName(series, name);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * 
     * Sets the title of the chart.
     * @param title - String for the name of the chart
     * @return BarChart
     */
    public BarChart setTitle(String title) {
        bbjBarChart.setTitle(title);
        return this;
    }

    /**
     * 
     * Sets the label or description of the X axis, will appear at the bottom of the chart.
     * @param title - String for the name of the X axis label
     * @return BarChart
     */
    public BarChart setXLabel(String label) {
        bbjBarChart.setXLabel(label); //Fixed copy/paste error here - MH
        return this;
    }

    /**
     * 
     * Sets the label or description of the Y axis, will appear at the bottom of the chart.
     * @param title - String for the name of the Y axis label
     * @return BarChart
     */
    public BarChart setYLabel(String label) {
        bbjBarChart.setYLabel(label);
        return this;
    }

    @Override
    public BarChart setStyle(String property, String value) {
        super.setControlStyle(property,value);
        return this;
    }

    @Override
    public BarChart addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public BarChart removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }
}
