package strbuilder

type llist struct {
	s    []byte
	next *llist
}

type Builder struct {
	head *llist
	curr *llist
}

func (this *Builder) Place(s string) {
	new := &llist{
		s:    []byte(s),
		next: nil,
	}
	if this.curr != nil {
		this.curr.next = new
	}
	this.curr = new
	if this.head == nil {
		this.head = new
	}
}

func (this *Builder) String() string {
	size := this.getSize()
	buff := make([]byte, size)
	index := 0
	curr := this.head
	for curr != nil {
		copy(buff[index:], curr.s)
		index += len(curr.s)
		curr = curr.next
	}
	return string(buff)
}

func (this *Builder) getSize() int {
	output := 0
	curr := this.head
	for curr != nil {
		output += len(curr.s)
		curr = curr.next
	}
	return output
}
