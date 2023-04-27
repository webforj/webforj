package org.dwcj.utilities;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;

public class DwcjImage extends Image {

  public static DwcjImage loadImageFromBytes(String binary) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromClientJar(String filename) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromClientUrl(String url) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromFile(String filename) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromFile(String filename, int index) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromImageList(int id, int index) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromServerJar(String filename) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromServerUrl(String url) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromUrl(String url) {
    return new DwcjImage();
  }


  @Override
  public int getWidth(ImageObserver observer) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'getWidth'");
  }

  @Override
  public int getHeight(ImageObserver observer) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'getHeight'");
  }

  @Override
  public ImageProducer getSource() {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'getSource'");
  }

  @Override
  public Graphics getGraphics() {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'getGraphics'");
  }

  @Override
  public Object getProperty(String name, ImageObserver observer) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'getProperty'");
  }



}