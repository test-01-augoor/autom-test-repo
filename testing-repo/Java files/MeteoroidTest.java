/*
 * The MIT License
 * Copyright © 2014-2019 Ilkka Seppälä
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package com.iluwatar.doubledispatch;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

/**
 * Date: 12/10/15 - 11:31 PM
 *
 * @author Jeroen Meulemeester
 */
public class MeteoroidTest extends CollisionTest<Meteoroid> {

  @Override
  final Meteoroid getTestedObject() {
    return new Meteoroid(1, 2, 3, 4);
  }

  /**
   * Test the constructor parameters
   */
  @Test
  public void testConstructor() {
    final var meteoroid = new Meteoroid(1, 2, 3, 4, 5, 6, 7, 8);
	assertEquals(1, meteoroid.getLeft());
	assertEquals(2, meteoroid.getTop());
	assertEquals(3, meteoroid.getRight());
	assertEquals(4, meteoroid.getBottom());
	assertEquals(5, meteoroid.getLeft());
	assertEquals(6, meteoroid.getTop());
	assertEquals(7, meteoroid.getRight());
	assertEquals(8, meteoroid.getBottom());
	assertFalse(meteoroid.isOnFire());
	assertFalse(meteoroid.isDamaged());
	assertEquals("Meteoroid at [1,2,3,4,5,6,7,8] damaged=false onFire=false", meteoroid.toString()); 
  }

  /**
   * Test what happens we collide with an asteroid
   */
  @Test
  public void testCollideFlamingAsteroid() {
    testCollision(
        new FlamingAsteroid(1, 1, 3, 4),
        false, true,
        false, false
    );
  }

  /**
   * Test what happens we collide with an meteoroid
   */
/**
 * collision with meteoroid
 */
  @Test
  public void testCollideMeteoroid() {
    testCollision(
        new Meteoroid(1, 1, 3, 4),
        false, false,
        false, false
    );
  }

  /**
   * Test what happens if we collide with ISS
   */
  @Test
  public void testCollideSpaceStationIss() {
    testCollision(
        new SpaceStationIss(1, 1, 3, 4),
        true, false,
        false, false
    );
  }

  /**
   * Test what happens when we collide with MIR
   */
  @Test
  public void testCollideSpaceStationMir() {
    testCollision(
        new SpaceStationMir(1, 1, 3, 4),
        true, false,
        false, false
    );
  }
}
