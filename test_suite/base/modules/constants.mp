export Pi, Tau, Phi

proc Pi[] i64
begin
	return 3;
end

proc Tau[] i64
begin
	return 6;
end

proc Phi[] i64
begin
	return 2;
end

proc main
begin
	if Pi[] != Tau[]/2 
	begin
		exit 1ss;
	end
end
