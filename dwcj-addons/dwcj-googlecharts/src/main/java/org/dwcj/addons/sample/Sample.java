package org.dwcj.addons.sample;

import org.dwcj.App;
import org.dwcj.addons.googlecharts.GoogleChart;
import org.dwcj.annotation.InlineStyleSheet;
import org.dwcj.component.window.Frame;
import org.dwcj.exceptions.DwcjException;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

@InlineStyleSheet(value = /* css */"""
    .app {
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 1rem;
      padding: 2rem;
      height: inherit
    }
    """)
public class Sample extends App {

  @Override
  public void run() throws DwcjException {
    Frame win = new Frame();
    win.addClassName("app");

    GoogleChart chart = new GoogleChart();
    chart.setType(GoogleChart.Type.PIE);
    chart.setStyle("width", "800px");
    chart.setStyle("height", "400px");

    // options
    // JsonObject options = new JsonObject();
    JsonObject options = new JsonObject();
    options.addProperty("title", "My Daily Activities");
    options.addProperty("is3D", true);
    options.addProperty("backgroundColor", "transparent");

    chart.setOptions(options);

    // data [cols, rows]
    JsonArray data = new JsonArray();

    // cols
    JsonArray columns = new JsonArray();
    columns.add("Task");
    columns.add("Hours per Day");
    data.add(columns);

    // rows
    String[] tasks = { "Work", "Eat", "Commute", "Watch TV", "Sleep" };
    int[] hours = { 11, 2, 2, 2, 7 };

    for (int i = 0; i < tasks.length; i++) {
      JsonArray row = new JsonArray();
      row.add(tasks[i]);
      row.add(hours[i]);
      data.add(row);
    }

    chart.setData(data);

    win.add(chart);
  }
}
