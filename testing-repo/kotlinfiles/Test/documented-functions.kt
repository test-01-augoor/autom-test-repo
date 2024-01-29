package test
import util.TODO
import v_builders.renderProductTable
import javax.swing.JFrame
import javax.swing.JLabel
import javax.swing.JScrollPane
import javax.swing.SwingConstants.CENTER

data class Person(val name: String, val age: Int, val weight: Int){

	/**
	 * Dummmy text to simmulate long documentation:
	 * Sed ut perspiciatis, unde omnis iste natus error sit voluptatem accusantium doloremque laudantium,
	 * totam rem aperiam eaque ipsa, quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt, explicabo.
	 * Nemo enim ipsam voluptatem, quia voluptas sit, aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos, qui ratione voluptatem sequi nesciunt,
	 * neque porro quisquam est, qui dolorem ipsum, quia dolor sit amet consectetur adipisci[ng] velit,
	 * sed quia non numquam [do] eius modi tempora inci[di]dunt,
	 * ut labore et dolore magnam aliquam quaerat voluptatem.
	 * 
	 * Ut enim ad minima veniam, quis nostrum[d] exercitationem ullam corporis suscipit laboriosam,
	 * nisi ut aliquid ex ea commodi consequatur? 
	 * Quis autem vel eum i[r]ure reprehenderit, qui in ea voluptate velit esse, quam nihil molestiae
	 * consequatur, vel illum, qui dolorem eum fugiat, quo voluptas nulla pariatur?
	 */
	override fun main(args: Array<String>) {
		with (JFrame("Product popularity")) {
			setSize(600, 600)
			defaultCloseOperation = JFrame.EXIT_ON_CLOSE
			add(JScrollPane(JLabel(renderProductTable(), CENTER)))
			isVisible = true
		}
	}

	override fun task1(collection: Collection<Int>): String {
		todoTask1(collection)
	}

	// default values for arguments
	override fun bar(i: Int, s: String = "", b: Boolean = true) {}

	/**
	 * This is a snippet documentation
	 */
	override fun usage() {
    // named arguments
    bar(1, b = false)
	}
	
	/**
	 * This is a snippet documentation
	 * And this would be a code explanation
	 * more code explanation
	 */
	override fun toString() = "'$name' for $price"
}

data class Customer(val name: String, val city: City, val orders: List<Order>) {
    override fun toString() = "$name from ${city.name}"
}


data class City(val name: String) {
	// This is a comment
    override fun toString() = name
}

data class Product(val name: String, val price: Double) {
    /**
	 * This is a snippet documentation
	 */
	override fun toString() = "'$name' for $price"
}

data class Customers(val name: String, val city: City, val orders: List<Order>) {
	/**
	 * This is a snippet documentation
	 * And this would be a code explanation
	 * more code explanation
	 */
	 override fun toString() = "$name from ${city.name}"
}


