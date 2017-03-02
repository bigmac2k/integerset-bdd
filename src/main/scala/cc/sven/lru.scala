package cc.sven

//https://github.com/papers-we-love/papers-we-love/blob/master/caching/a-constant-algorithm-for-implementing-the-lfu-cache-eviction-scheme.pdf

import scala.collection.mutable

trait Identifier[Item, Id] {

    def apply(item: Item): Id

}

class HashCodeIdentifier[Item] extends Identifier[Item, Int] {

    override def apply(item: Item): Int = item.hashCode

}

class DoubleLinkedLFUCache[Item, Id](maxSize: Int) {

    private val entries = mutable.HashMap.empty[Id, CachedItem]
    private val frequencyList = new FrequencyList(0)


    def insert(id: Id, item: Item): Unit = {
        if (entries.contains(id)) lookup(id)
        else append(id, item)
    }

    def lookup(itemId: Id): Option[Item] = {
        entries.get(itemId).map { cachedItem =>
            cachedItem.bump()
            cachedItem.item
        }
    }

    def getOrElseUpdate(key: Id, op: => Item): Item =
        lookup(key) match {
            case Some(v) => v
            case None => val d = op; insert(key, d); d
        }

    def removeLast(): Option[Item] = {
        println("removeLast")
        if (entries.isEmpty) return None

        val last = frequencyList.next.removeLast()
        entries.remove(last.key)
        Some(last.item)
    }

    private def append(id: Id, item: Item): Unit = {
        if (entries.size == maxSize) removeLast()

        val cachedItem = new CachedItem(id, item)
        frequencyList.add(cachedItem)
        cachedItem.bump()
        entries.put(id, cachedItem)
    }

    class FrequencyList(val frequency: Int) extends DoubleLinked[FrequencyList] {

        var previous = this
        var next = this

        var items: CachedItem = null

        def bump(item: CachedItem): Unit = {
        val bumpedFrequency = frequency + 1
        val linked =
                if (next.frequency == bumpedFrequency) next
                else link(new FrequencyList(bumpedFrequency))

        remove(item)
        linked.add(item)
    }

        def link(link: FrequencyList): FrequencyList = {
        link.next = next
        link.previous = this
        next.previous = link
        next = link
        link
    }

        def unlink(link: FrequencyList): FrequencyList = {
        link.previous.next = link.next
        link.next.previous = link.previous
        link
    }

        def add(item: CachedItem): Unit = {
        item.list = this
        if (items == null) item.reset()
        else items.addBefore(item)
        items = item
    }

        def remove(item: CachedItem): Unit = {
        if (frequency == 0) items = null
        else if (item.isSingle) unlink(this)
        else item.remove()

        if (items == item) items = item.next
    }

        def removeLast(): CachedItem = {
        if (items.isSingle) unlink(this).items
        else items.last.remove()
    }

    }

    class CachedItem(val key: Id, val item: Item) extends DoubleLinked[CachedItem] {

        var list: FrequencyList = null

        var previous = this
        var next = this

        def isSingle: Boolean =
        next == this

        def addBefore(item: CachedItem): Unit = {
        item.previous = previous
        item.next = this
        previous.next = item
        previous = item
    }

        def remove(): CachedItem = {
        previous.next = next
        next.previous = previous
        this
    }

        def reset(): Unit = {
        previous = this
        next = this
    }

        def bump(): Unit = {
        list.bump(this)
    }

        def last: CachedItem =
        previous

    }

    trait DoubleLinked[Type <: DoubleLinked[Type]] { self: Type =>

        def next: Type
                def previous: Type

                def iterate(f: Type => Unit): Unit = {
            var tail = this
            do {
                f(tail)
                tail = tail.next
            } while (tail != this)
        }

    }

    override def toString: String = {
        val str = StringBuilder.newBuilder
        str.append("LFU")
        frequencyList.iterate { list =>
            str.append("\n |\n[").append(list.frequency).append("]")
            if (list.items != null) {
                list.items.iterate { cached =>
                    str.append("-").append(cached.key)
                }
            }
        }
        str.toString()
    }

}

