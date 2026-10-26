package com.webforj.automation.see.component.googlecharts;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.component.googlecharts.GoogleChart;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class GoogleChartSnapshotSerializerTest {

  @Test
  void shouldPrintTheTypeAndTheTitleByDefault() {
    GoogleChart chart = new GoogleChart(GoogleChart.Type.COLUMN);
    chart.setOptions(Map.of("title", "Monthly revenue"));

    assertEquals("- GoogleChart \"Monthly revenue\" [type=column]\n", Snapshot.render(chart));
  }

  @Test
  void shouldPrintOnlyTheTypeWithoutTitle() {
    GoogleChart chart = new GoogleChart(GoogleChart.Type.PIE);

    assertEquals("- GoogleChart [type=pie]\n", Snapshot.render(chart));
  }

  @Test
  void shouldPrintTheRowsOfTheDataUnderTheirColumnNames() {
    GoogleChart chart = new GoogleChart(GoogleChart.Type.COLUMN);
    chart.setData(List.of(List.of("Year", "Sales"), List.of("2024", 1000), List.of("2025", 1170)));

    assertEquals("""
        - GoogleChart [type=column]:
          - Row [Year=2024] [Sales=1000]
          - Row [Year=2025] [Sales=1170]
        """, Snapshot.render(chart));
  }

  @Test
  void shouldPrintTheRowsUnderTheLabelOrTheIdOrTheIndexOfDeclaredColumns() {
    GoogleChart chart = new GoogleChart(GoogleChart.Type.CANDLESTICK);
    chart.setData(List.of(
        List.of(Map.of("type", "string", "label", "Day", "id", "day"),
            Map.of("type", "number", "id", "low"), Map.of("type", "number")),
        List.of("Mon", 20, 28)));

    assertEquals("""
        - GoogleChart [type=candlestick]:
          - Row [Day=Mon] [low=20] [2=28]
        """, Snapshot.render(chart));
  }
}
