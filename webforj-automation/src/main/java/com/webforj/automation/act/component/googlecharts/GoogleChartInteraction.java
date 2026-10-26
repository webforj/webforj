package com.webforj.automation.act.component.googlecharts;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.SelectInteraction;
import com.webforj.component.googlecharts.GoogleChart;
import com.webforj.component.googlecharts.events.GoogleChartSelectedEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Selects in charts, the elements of a list or else the given item, before the chart reports it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class GoogleChartInteraction extends AbstractComponentInteraction<GoogleChart>
    implements SelectInteraction<GoogleChart, Object> {

  /**
   * Creates the interaction.
   */
  public GoogleChartInteraction() {
    super(GoogleChart.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void select(GoogleChart chart, Object item) {
    List<Object> selection = new ArrayList<>(item instanceof List<?> items ? items : List.of(item));
    chart.setSelection(selection);

    Map<String, Object> data = new HashMap<>();
    data.put("selection", new ArrayList<>(selection));
    fire(new GoogleChartSelectedEvent(chart, data));
  }
}
