package com.webforj.data.transformation.transformer;

import com.webforj.data.transformation.TransformationException;
import java.time.LocalTime;

/**
 * Represents a transformer time value in hours and fractional hours and a java LocalTime.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public class HoursLocalTimeTransformer implements Transformer<Double, LocalTime> {

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalTime transformToModel(Double viewValue) {
    try {
      LocalTime time = null;
      if (Double.isFinite(viewValue) && viewValue >= 0) {
        time = LocalTime.ofSecondOfDay((long) (viewValue * 3600));
      }

      return time;
    } catch (Exception e) {
      throw new TransformationException("Error transforming component value to model value", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Double transformToComponent(LocalTime modelValue) {
    try {
      Double tim = Double.NaN;
      if (modelValue != null) {
        int hour = modelValue.getHour();
        int minute = modelValue.getMinute();
        int second = modelValue.getSecond();
        int millisecond = (int) (modelValue.toNanoOfDay() % 1_000_000);


        if (hour < 0 || minute < 0 || second < 0 || millisecond < 0) {
          throw new TransformationException("Time components must be non-negative integers");
        }

        tim = hour + (minute / 60.0) + (second / 3600.0) + (millisecond / 3600000.0);
      }

      return tim;
    } catch (Exception e) {
      throw new TransformationException("Error transforming model value to component value", e);
    }
  }
}
