<?xml version="1.0" encoding="UTF-8"?>
<tileset version="1.10" tiledversion="1.10.1" name="single1" tilewidth="16" tileheight="12" spacing="1" margin="2" tilecount="8" columns="4">
 <image source="tileset.png" width="70" height="36"/>
 <tile id="0"/>
 <tile id="1">
  <animation>
   <frame tileid="1" duration="600"/>
   <frame tileid="2" duration="100"/>
   <frame tileid="3" duration="200"/>
   <frame tileid="2" duration="100"/>
  </animation>
 </tile>
 <tile id="2"/>
 <tile id="3"/>
 <tile id="5"/>
 <tile id="4"/>
 <tile id="6">
  <objectgroup draworder="index" id="5">
   <object id="4" x="0.248898" y="0.165932" width="7.92326" height="7.92326">
    <ellipse/>
   </object>
   <object id="5" x="15.3902" y="5.60021">
    <polygon points="0,0 -4.48017,6.098 0.248898,6.05652"/>
   </object>
   <object id="7" x="0" y="0" width="16" height="12"/>
   <object id="8" x="3.52606" y="-14.8509">
    <point/>
   </object>
  </objectgroup>
 </tile>
 <tile id="7"/>
</tileset>
