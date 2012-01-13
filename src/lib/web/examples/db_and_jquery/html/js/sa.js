
  function delete_sa(ip,id,fname,lname) {
    $.getJSON('delete_sa?ip='+$( "#ip" ).text()+"&id="+id,update_sa_list);
    log("Deleted: " + fname +" "+ lname + " as system administrator");
   }

   function add_sa(id,name) {
     $.getJSON('add_sa?ip='+$( "#ip" ).text()+"&id="+id,update_sa_list);
   }

   function open_sa_dialog(id) {
     $.getJSON('view_sa_detail?id='+id,function(data) {
       $.each(data, function(key, val) {
       $('#dialog_body').empty();
       $('<div>',{
         html:"<table>"+
              "<tr><th>Name:</th><td>"+val[1]+" "+val[2]+"</td></tr>"+
              "<tr><th>Phone:</th><td>"+val[3]+"</td></tr>"+
              "<tr><th>Location:</th><td>"+val[4]+"</td></tr>"+
              "<tr><th>Bldg/Room:</th><td>"+val[5]+" "+val[6]+"</td></tr>"
         }).appendTo('#dialog_body');
        });
        $( "#dialog" ).dialog( "open" );
	return false;
      });
    }

    function update_sa_list (data) {
      var items = [];
      $('#sas').empty();
      $.each(data, function(key, val) {
        items.push('<tr><td><span id="' + key + '"class="icons '+
	           'ui-corner-all" onclick="delete_sa('+"'"+val[0]+"','"+val[1]+"'"+
                   ",'"+val[2]+"','"+val[3]+"'"+')" title="Delete Administrator">'+
                   '<span class="ui-icon '+
                   'ui-icon-minusthick"></span></td><td>'+
                   '<span class="sa_itm" id="'+val[1]+'" title="SA Detail" onclick="open_sa_dialog('+"'"+val[1]+"'"+
                   ')">'+val[2]+' '+val[3]+
                   '</span></td></tr>' );
       });
       $('<table/>', {
         'class': 'ui-widget ui-helper-clearfix ',
          html: items.join('')
       }).appendTo('#sas');
     }

     function get_sas () {
       $.getJSON('view_sa?ip='+$( "#ip" ).text(),update_sa_list);
     }

     function log( message ) {
       $( "<div/>" ).text( message ).prependTo( "#log" );
        $( "#log" ).scrollTop( 0 );
     }
    
     $(function() {
	$( "#sa" ).autocomplete({
          source: "search_sa",
          minLength: 2,
          select: function( event, ui ) {
            add_sa(ui.item.id,ui.item.value);//ui.item.id             
	    log( ui.item ?
	      "Added: " + ui.item.value + " as system administrator" :
	      "Unable to add " + this.value );
	  }
	});
        $( "#dialog" ).dialog({
			autoOpen: false,
			show: "blind",
			hide: "explode"
        });
        get_sas();
      });